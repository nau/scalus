package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.Builtins.blake2b_224
import scalus.builtin.ByteString
import scalus.builtin.ByteString.*
import scalus.builtin.Data.toData
import scalus.builtin.ToDataInstances.given
import scalus.ledger.api.v1.Credential.{PubKeyCredential, ScriptCredential}
import scalus.ledger.api.v1.{Address, Credential, PubKeyHash, Value}
import scalus.ledger.api.v2.TxOut
import scalus.ledger.api.v3.*
import scalus.ledger.api.v3.ScriptInfo.SpendingScript
import scalus.ledger.api.v3.ToDataInstances.given
import scalus.prelude.{List, Option, *}
import scalus.uplc.*
import scalus.uplc.eval.*

class PaymentSplitterSpec extends AnyFunSuite, ScalusTest {

    TestCases(
      TestCase(
        description = "success when payments are correctly split for a single payee",
        payees = List(Payee.A),
        amount = 30,
        fee = 2,
        inputs = List(Payee.A give 10),
        outputs = List(Payee.A get 38),
        expected = success
      ),
      TestCase(
        description = "success when payments are correctly split for between 2 payees",
        payees = List(Payee.A, Payee.B),
        amount = 30,
        fee = 2,
        inputs = List(Payee.A give 10),
        outputs = List(Payee.A get 23, Payee.B get 15),
        expected = success
      ),
      TestCase(
        description = "success when payments are correctly split for between 3 payees",
        payees = List(Payee.A, Payee.B, Payee.C),
        amount = 30,
        fee = 2,
        inputs = List(Payee.A give 10),
        outputs = List(Payee.A get 18, Payee.B get 10, Payee.C get 10),
        expected = success
      )
    )

    private val A = hex"1234567890abcdef1234567890abcdef1234567890abcdef12345678"
    private val B = hex"2234567890abcdef1234567890abcdef1234567890abcdef12345678"
    private val C = hex"3234567890abcdef1234567890abcdef1234567890abcdef12345678"

    test("success when payments are correctly split for a single payee") {
        val payees = List(A)
        assertCase(
          payees,
          inputs = List(
            makePayeeInput(pkh = A, idx = 0, value = 100),
            makeScriptInput(100)
          ),
          outputs = List((A, 190)),
          fee = 10,
          expected = success(ExBudget(ExCPU(108268790), ExMemory(454315)))
        )
    }

    test("failure when a payee is not present in the inputs") {
        val payees = List(A)
        assertCase(
          payees,
          inputs = List(makeScriptInput(100)),
          outputs = List.empty,
          fee = 10,
          expected =
              failure("One of the payees must have an input to pay the fee and trigger the payout")
        )
    }

    test("failure when a payee is not payed out") {
        val payees = List(A)
        assertCase(
          payees,
          inputs = List(
            makeScriptInput(100),
            makePayeeInput(pkh = A, idx = 0, value = 100)
          ),
          outputs = List.empty,
          fee = 10,
          expected = failure("Not all payees were paid")
        )
    }

    test("failure when multiple payees are present in the inputs") {
        val payees = List(A, B)
        assertCase(
          payees,
          inputs = List(
            makeScriptInput(100),
            makePayeeInput(pkh = A, idx = 0, value = 100),
            makePayeeInput(pkh = B, idx = 1, value = 100)
          ),
          outputs = List.empty,
          fee = 10,
          expected = failure("Already found a fee payer")
        )
    }

    test("success when multiple payees are correctly split") {
        val payees = List(A, B)
        assertCase(
          payees,
          inputs = List(
            makeScriptInput(100),
            makePayeeInput(pkh = A, idx = 0, value = 100)
          ),
          outputs = List((A, 50 + 100 - 10), (B, 50)),
          fee = 10,
          expected = success(ExBudget(ExCPU(130365073), ExMemory(557236)))
        )
    }

    test("failure when extra outputs are present") {
        val payees = List(A, B)
        assertCase(
          payees,
          inputs = List(
            makeScriptInput(100),
            makePayeeInput(pkh = A, idx = 0, value = 100)
          ),
          outputs = List(
            (A, 50 + 100 - 10),
            (B, 50),
            (C, 50) // extra output
          ),
          fee = 10,
          expected = failure("More outputs than payees")
        )
    }

    test("failure when not all payees are payed out") {
        val payees = List(A, B)
        assertCase(
          payees,
          inputs = List(
            makeScriptInput(100),
            makePayeeInput(pkh = A, idx = 0, value = 100)
          ),
          outputs = List(
            (A, 50 + 100 - 10)
          ),
          fee = 10,
          expected = failure("Not all payees were paid")
        )
    }

    private val script = compile(PaymentSplitter.validate)
        .toUplc(generateErrorTraces = true)
        .plutusV3

    private val aikenScript = {
        import upickle.default.*
        val obj = read[ujson.Obj](this.getClass.getResourceAsStream("/plutus.json"))
        val cborHex = obj("validators").arr(0)("compiledCode").str
        DeBruijnedProgram.fromCborHex(cborHex)
    }

    private val lockTxId = random[TxId]
    private val payeesTxId = random[TxId]
    private val txId = random[TxId]
    private val scriptHash = blake2b_224(ByteString.fromArray(3 +: script.cborEncoded))

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

//        PaymentSplitter.validator(List(payees).toData)(context.toData)

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
                          clue = s"Expected success, but got: ${result.toString}"
                        )
                    case Option.Some(budget) =>
                        assert(
                          result.isSuccess,
                          clue =
                              s"Expected success with budget: $budget, but got: ${result.toString}"
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
        inline infix def give(amount: BigInt): Input = Input(payee, amount)
        inline infix def get(amount: BigInt): Output = Output(payee, amount)

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

    def failure(message: String): Either[String, Option[ExBudget]] = Left(message)
    def success: Either[String, Option[ExBudget]] = Right(Option.None)
    def success(budget: ExBudget): Either[String, Option[ExBudget]] = Right(Option.Some(budget))
}
