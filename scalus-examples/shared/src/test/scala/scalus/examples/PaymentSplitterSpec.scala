package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.builtin.ToDataInstances.given
import scalus.ledger.api.v1.Credential.{PubKeyCredential, ScriptCredential}
import scalus.ledger.api.v1.{Address, Credential, PubKeyHash, Value}
import scalus.ledger.api.v2.TxOut
import scalus.ledger.api.v3.*
import scalus.ledger.api.v3.ScriptInfo.SpendingScript
import scalus.ledger.api.v3.ToDataInstances.given
import scalus.prelude.{List, Option, *}
import scalus.testkit.*
import scalus.uplc.*
import scalus.uplc.eval.*

import scala.util.control.NonFatal

class PaymentSplitterSpec extends AnyFunSuite, ScalusTest {
    import Payee.*

    TestCases(
      TestCase(
        description = "success when payments are correctly split for a single payee",
        payees = List(A),
        amount = 30,
        fee = 2,
        inputs = List(A gives 10),
        outputs = List(A gets 38),
        expected = success
      ),
      TestCase(
        description = "success when payments are correctly split between 2 payees",
        payees = List(A, B),
        amount = 30,
        fee = 2,
        inputs = List(A gives 10),
        outputs = List(A gets 23, B gets 15),
        expected = success
      ),
      TestCase(
        description = "success when payments are correctly split between 3 payees",
        payees = List(A, B, C),
        amount = 30,
        fee = 2,
        inputs = List(A gives 10),
        outputs = List(A gets 18, B gets 10, C gets 10),
        expected = success
      ),
      TestCase(
        description = "failure when a payee is not present in the inputs",
        payees = List(A, B),
        amount = 30,
        fee = 2,
        inputs = List.empty,
        outputs = List(A gets 14, B gets 14),
        expected = failure(
          "One of the payees must have an input to pay the fee and trigger the payout"
        )
      ),
      TestCase(
        description = "failure when a payee is not payed out (1 payee)",
        payees = List(A),
        amount = 30,
        fee = 2,
        inputs = List(A gives 10),
        outputs = List.empty,
        expected = failure("Not all payees were paid")
      ),
      TestCase(
        description = "failure when a one of the payee is not payed out",
        payees = List(A, B),
        amount = 30,
        fee = 2,
        inputs = List(A gives 10),
        outputs = List(A gets 38),
        expected = failure("Not all payees were paid")
      ),
      TestCase(
        description = "failure when payee not present in contact to be payed",
        payees = List(A, B),
        amount = 30,
        fee = 2,
        inputs = List(A gives 10),
        outputs = List(A gets 18, B gets 10, C gets 10),
        expected = failure("More outputs than payees")
      ),
      TestCase(
        description = "success when split equally and remainder compensates fee - o1",
        payees = List(A, B, C),
        amount = 31,
        fee = 2,
        inputs = List(A gives 3),
        outputs = List(A gets 12, B gets 10, C gets 10),
        expected = success
      ),
      TestCase(
        description = "success when split equally and remainder compensates fee - o2",
        payees = List(A, B, C),
        amount = 31,
        fee = 3,
        inputs = List(A gives 3),
        outputs = List(A gets 11, B gets 10, C gets 10),
        expected = success
      ),
      TestCase(
        description = "success when split equally and remainder compensates fee - o3",
        payees = List(A, B, C),
        amount = 31,
        fee = 4,
        inputs = List(A gives 3),
        outputs = List(A gets 10, B gets 10, C gets 10),
        expected = success
      ),
      TestCase(
        description = "failure when inflated fee reduces the split payout",
        payees = List(A, B, C),
        amount = 31,
        fee = 10,
        inputs = List(A gives 3),
        outputs = List(A gets 8, B gets 8, C gets 8),
        expected = failure("value to be payed to payees is too low")
      )
    )

    test("failure when multiple payees are present in the inputs") {
        val payees = List(A.pkh, B.pkh)
        assertCase(
          payees,
          inputs = List(
            makeScriptInput(100),
            makePayeeInput(pkh = A.pkh, idx = 0, value = 100),
            makePayeeInput(pkh = B.pkh, idx = 1, value = 100)
          ),
          outputs = List.empty,
          fee = 10,
          expected = failure("Already found a fee payer")
        )
    }

    test("success when multiple payees are correctly split") {
        val payees = List(A.pkh, B.pkh)
        assertCase(
          payees,
          inputs = List(
            makeScriptInput(100),
            makePayeeInput(pkh = A.pkh, idx = 0, value = 100)
          ),
          outputs = List((A.pkh, 50 + 100 - 10), (B.pkh, 50)),
          fee = 10,
          expected = success
        )
    }

    private val script = {
        try {
            compile(PaymentSplitter.validate)
                .toUplc(generateErrorTraces = true)
                .plutusV3
        } catch {
            case NonFatal(ex) =>
                println("Can't compile script PaymentSplitter.validate")
                ex.printStackTrace()
                throw ex
        }
    }

    /*
    private val aikenScript = {
        import upickle.default.*
        val obj = read[ujson.Obj](this.getClass.getResourceAsStream("/plutus.json"))
        val cborHex = obj("validators").arr(0)("compiledCode").str
        DeBruijnedProgram.fromCborHex(cborHex)
    }

     */

    private val lockTxId = random[TxId]
    private val payeesTxId = random[TxId]
    private val txId = random[TxId]
    private val scriptHash = script.hash

    private val runScalaVersion = true

    private def assertCase(
        payees: List[ByteString],
        inputs: List[TxInInfo],
        outputs: List[(ByteString, BigInt)],
        fee: BigInt,
        expected: Either[String, Option[ExBudget]]
    ): Unit = assertCase(script)(payees, inputs, outputs, fee, expected)

    private def assertCase(script: Program)(
        payees: List[ByteString],
        inputs: List[TxInInfo],
        outputs: List[(ByteString, BigInt)],
        fee: BigInt,
        expected: Either[String, Option[ExBudget]]
    ): Unit = {
        // Create script with payees parameter
        val applied = script $ List(payees).toData

        // Build transaction outputs from provided parameters
        val txOutputs = outputs.map { case (pkh, amount) =>
            TxOut(
              address = Address(
                PubKeyCredential(PubKeyHash(pkh)),
                Option.None
              ),
              value = Value.lovelace(amount)
            )
        }

        // Create script context with given inputs, outputs and fee
        val context = ScriptContext(
          txInfo = TxInfo(inputs = inputs, outputs = txOutputs, fee = fee, id = txId),
          scriptInfo = SpendingScript(txOutRef = TxOutRef(lockTxId, 0))
        )

        // Apply script to the context
        val program = applied $ context.toData

        if runScalaVersion then
            try PaymentSplitter.validate(List(payees.toData).toData)(context.toData)
            catch
                case NonFatal(ex) =>
                    if expected.isRight then throw ex

        // Evaluate the program
        val result = program.evaluateDebug

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
            address = Address(PubKeyCredential(PubKeyHash(pkh)), Option.None),
            value = Value.lovelace(value)
          )
        )
    }

    private def makeScriptInput(value: BigInt): TxInInfo = {
        TxInInfo(
          outRef = TxOutRef(lockTxId, 0),
          resolved = TxOut(
            address = Address(ScriptCredential(scriptHash), Option.None),
            value = Value.lovelace(value)
          )
        )
    }

    enum Payee(val pkh: ByteString):
        case A extends Payee(genByteStringOfN(28).sample.get)
        case B extends Payee(genByteStringOfN(28).sample.get)
        case C extends Payee(genByteStringOfN(28).sample.get)
        case D extends Payee(genByteStringOfN(28).sample.get)
        case E extends Payee(genByteStringOfN(28).sample.get)
        case F extends Payee(genByteStringOfN(28).sample.get)
        case G extends Payee(genByteStringOfN(28).sample.get)
        case H extends Payee(genByteStringOfN(28).sample.get)

    extension (payee: Payee)
        inline infix def gives(amount: BigInt): Input = Input(payee, amount)
        inline infix def gets(amount: BigInt): Output = Output(payee, amount)

    case class Input(payee: Payee, amount: BigInt)

    case class Output(payee: Payee, amount: BigInt)

    case class TestCase(
        description: String,
        payees: List[Payee],
        amount: BigInt,
        fee: BigInt,
        inputs: List[Input],
        outputs: List[Output],
        expected: Either[String, Option[ExBudget]]
    )

    object TestCases {
        def apply(cases: TestCase*): Unit = {
            cases.zipWithIndex.foreach {
                case (
                      TestCase(description, payees, amount, fee, inputs, outputs, expected),
                      caseIndex
                    ) =>
                    test(s"${caseIndex + 1}: $description") {
                        assertCase(
                          payees.map(_.pkh),
                          inputs = List.from(
                            inputs.asScala.zipWithIndex
                                .map { case (Input(payee, amount), index) =>
                                    makePayeeInput(payee.pkh, index, amount)
                                }
                                .prepended(makeScriptInput(amount))
                          ),
                          outputs = outputs.map { case Output(payee, amount) =>
                              (payee.pkh, amount)
                          },
                          fee = fee,
                          expected = expected
                        )
                    }
            }
        }
    }
}
