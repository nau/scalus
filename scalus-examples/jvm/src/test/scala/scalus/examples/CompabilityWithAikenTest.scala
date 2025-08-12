package scalus.examples

import scalus.*
import scalus.prelude.*
import scalus.builtin.ByteString
import scalus.builtin.ToData
import scalus.builtin.FromData
import scalus.builtin.Data.toData
import scalus.uplc.DeBruijnedProgram
import scalus.uplc.eval.{ExBudget, ExCPU, ExMemory}
import scalus.uplc.eval.Result
import org.scalatest.funsuite.AnyFunSuite
import scalus.testkit.*
import scalus.ledger.api.v3.*

/** Aiken require that redeemper is ADT with constructor with arguments.
  * @param message
  */
case class PaymentSplitterRedeemer(message: String)

@scalus.Compile
object PaymentSplitterRedeemer {
    given ToData[PaymentSplitterRedeemer] = ToData.derived
    given FromData[PaymentSplitterRedeemer] = FromData.derived
}

case class PaymentSplitterDatum(owner: ByteString)

@scalus.Compile
object PaymentSplitterDatum {
    given ToData[PaymentSplitterDatum] = ToData.derived
    given FromData[PaymentSplitterDatum] = FromData.derived
}

class CompabilityWithAikenTest extends AnyFunSuite, ScalusTest {

    import Payee.*

    test("run aiken-compiled test with two payeers in the outputs") {
        val payees = List(A.pkh, B.pkh, C.pkh, D.pkh, E.pkh)
        assertCase(
          payees,
          inputs = List(
            makePayeeInput(A.pkh, idx = 0, value = 41961442),
            makeScriptInput(15000000),
          ),
          outputs = List(
            (A.pkh, 3000000),
            (A.pkh, 41115417),
            (B.pkh, 3000000),
            (C.pkh, 3000000),
            (D.pkh, 3000000),
            (E.pkh, 3000000)
          ),
          fee = 846025,
          extraSignatories = List(
            ByteString.fromHex("52bd00e69e371daa373c021c3f4321356902016c320e41526e239406")
          ),
          expected = Right(Option.None)
        )
    }

    test("run aiken-compiled test with merged outputs") {
        val payees = List(A.pkh, B.pkh, C.pkh, D.pkh, E.pkh)
        assertCase(
          payees,
          inputs = List(
            makePayeeInput(A.pkh, idx = 0, value = 41961442),
            makeScriptInput(15000000),
          ),
          outputs = List(
            (A.pkh, 3000000 + 41115417),
            (B.pkh, 3000000),
            (C.pkh, 3000000),
            (D.pkh, 3000000),
            (E.pkh, 3000000)
          ),
          fee = 846025,
          extraSignatories = List(
            ByteString.fromHex("52bd00e69e371daa373c021c3f4321356902016c320e41526e239406")
          ),
          expected = Right(Option.None)
        )
    }

    enum Payee(val pkh: ByteString):
        case A
            extends Payee(
              ByteString.fromHex("92e2ae51fb03dcc55c471506fe35bdedad9c266b0d09c2b8bc7cb445")
            )
        case B
            extends Payee(
              ByteString.fromHex("3cea9e9f66482d00cdacd2087d27173d534b76ce7e3cc86e8a07f434")
            )
        case C
            extends Payee(
              ByteString.fromHex("01f3855a5b36569e6ce1bdd8286f2b8b4e764b0e1a90a3e3d8357490")
            )
        case D
            extends Payee(
              ByteString.fromHex("ce63d3ad78867ac366cabe17e3f15f76d05e0f99827986f92413c4d8")
            )
        case E
            extends Payee(
              ByteString.fromHex("06cae4f91a7e73521cfb42a0dcfe0d90feff52ffa096467995bfb503")
            )

    private lazy val aikenScript = {
        import upickle.default.*
        val fname = "/scalus/examples/PaymentSplitterAikenData/plutus.json"
        val inputStream = this.getClass.getResourceAsStream(fname)
        if inputStream == null then {
            println(s"Can't load script from $fname")
            throw new RuntimeException(s"Resource not found: $fname")
        }
        val obj = read[ujson.Obj](inputStream)
        val cborHex = obj("validators").arr(0)("compiledCode").str
        val program = DeBruijnedProgram.fromCborHex(cborHex).toProgram
        program
    }

    private val lockTxId = random[TxId]
    private val payeesTxId = random[TxId]
    private val txId = random[TxId]
    private val scriptHash = aikenScript.hash

    private def assertCase(
        payeesBytes: List[ByteString],
        inputs: List[TxInInfo],
        outputs: List[(ByteString, BigInt)],
        fee: BigInt,
        extraSignatories: List[ByteString],
        expected: Either[String, Option[ExBudget]]
    ): Unit = {
        // Create script with payees parameter

        val payees = payeesBytes.map(PubKeyHash(_))

        val payeesData = payees.toData

        // Build transaction outputs from provided parameters
        val txOutputs = outputs.map { case (pkh, amount) =>
            TxOut(
              address = Address(
                Credential.PubKeyCredential(PubKeyHash(pkh)),
                Option.None
              ),
              value = Value.lovelace(amount)
            )
        }

        // Create script context with given inputs, outputs and fee
        // val pkh = PubKeyHash(
        //  ByteString.fromHex("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        // )
        // val txCert = TxCert.RegStaking(Credential.PubKeyCredential(pkh), Option.None)
        val txOutRef = TxOutRef(lockTxId, 0)
        val redeemer = PaymentSplitterRedeemer("qqq").toData
        val context = ScriptContext(
          txInfo = TxInfo(
            inputs = inputs,
            outputs = txOutputs,
            fee = fee,
            signatories = extraSignatories.map(x => PubKeyHash(x)),
            redeemers = SortedMap.fromList(
              scalus.prelude.List((ScriptPurpose.Spending(txOutRef), redeemer))
            ),
            id = txId
          ),
          redeemer = redeemer,
          scriptInfo = ScriptInfo.SpendingScript(txOutRef = txOutRef),
        )

        val aikenApplied = aikenScript $ payeesData
        val aikenWithContext = aikenApplied $ context.toData

        val result = aikenWithContext.evaluateDebug

        // Assert the result matches the expected outcome
        expected match
            case Left(errorMsg) =>
                assert(
                  result.isFailure,
                  clue = s"Expected failure with: $errorMsg, but got success"
                )
                // If a specific error message is provided, check it matches
                assert(
                  result.logs.exists(_.contains(errorMsg)),
                  clue =
                      s"Expected error containing: $errorMsg, but got: ${result.logs.mkString(", ")}"
                )
            case Right(success) =>
                success match
                    case Option.None =>
                        result match
                            case Result.Failure(ex, budget, cost, logs) =>
                                if logs.nonEmpty then println(s"logs=${logs}")
                                ex match
                                    case be: scalus.uplc.eval.BuiltinError =>
                                        be.cause.printStackTrace()
                                    case _ =>
                                        ex.printStackTrace()
                            case Result.Success(term, budget, costs, logs) =>
                                if logs.nonEmpty then println(s"logs=$logs")
                        assert(
                          result.isSuccess,
                          clue =
                              s"Expected success, but got: ${result.toString}, logs0: ${result.logs.mkString(", ")}"
                        )
                    case Option.Some(budget) =>
                        assert(
                          result.isSuccess,
                          clue =
                              s"Expected success with budget: $budget, but got: ${result.toString}, logs0: ${result.logs
                                      .mkString(", ")}"
                        )
                        if budget != ExBudget(ExCPU(0), ExMemory(0))
                        then // Check if budget verification is requested
                            assert(
                              result.budget == budget,
                              clue = s"Expected budget: $budget, but got: ${result.budget}"
                            )
    }

    private def makePayeeInput(pkh: ByteString, idx: Int, value: BigInt): TxInInfo = {
        TxInInfo(
          outRef = TxOutRef(payeesTxId, idx),
          resolved = TxOut(
            address = Address(Credential.PubKeyCredential(PubKeyHash(pkh)), Option.None),
            value = Value.lovelace(value)
          )
        )
    }

    private def makeScriptInput(value: BigInt): TxInInfo = {
        TxInInfo(
          outRef = TxOutRef(lockTxId, 0),
          resolved = TxOut(
            address = Address(Credential.ScriptCredential(scriptHash), Option.None),
            value = Value.lovelace(value)
          )
        )
    }

}
