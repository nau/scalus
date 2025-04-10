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

    private val A = hex"1234567890abcdef1234567890abcdef1234567890abcdef12345678"
    private val B = hex"2234567890abcdef1234567890abcdef1234567890abcdef12345678"
    private val C = hex"3234567890abcdef1234567890abcdef1234567890abcdef12345678"

    test("success when payments are correctly split for a single payee") {
        val payees = List(A)
        assertCase(
          payees,
          inputs = List(
            makePayeeTxInInfo(pkh = payees.head, idx = 0, value = 100),
            makeScriptTxInInfo(100)
          ),
          outputs = List((payees.head, 190)),
          fee = 10,
          expected = Right(ExBudget(ExCPU(107580594), ExMemory(451911)))
        )
    }

    test("failure when a payee is not present in the inputs") {
        val payees = List(A)
        assertCase(
          payees,
          inputs = List(makeScriptTxInInfo(100)),
          outputs = List.empty,
          fee = 10,
          expected =
              Left("One of the payees must have an input to pay the fee and trigger the payout")
        )
    }

    test("failure when a payee is not payed out") {
        val payees = List(A)
        assertCase(
          payees,
          inputs = List(
            makeScriptTxInInfo(100),
            makePayeeTxInInfo(pkh = payees.head, idx = 0, value = 100)
          ),
          outputs = List.empty,
          fee = 10,
          expected = Left("Not all payees were paid")
        )
    }

    test("failure when multiple payees are present in the inputs") {
        val payees = List(A, B)
        assertCase(
          payees,
          inputs = List(
            makeScriptTxInInfo(100),
            makePayeeTxInInfo(pkh = payees.head, idx = 0, value = 100),
            makePayeeTxInInfo(pkh = payees !! 1, idx = 1, value = 100)
          ),
          outputs = List.empty,
          fee = 10,
          expected = Left("Already found a fee payer")
        )
    }

    test("success when multiple payees are correctly split") {
        val payees = List(A, B)
        assertCase(
          payees,
          inputs = List(
            makeScriptTxInInfo(100),
            makePayeeTxInInfo(pkh = payees.head, idx = 0, value = 100)
          ),
          outputs = List((payees.head, 50 + 100 - 10), (payees !! 1, 50)),
          fee = 10,
          expected = Right(ExBudget(ExCPU(130348877), ExMemory(559032)))
        )
    }

    test("failure when extra outputs are present") {
        val payees = List(A, B)
        assertCase(
          payees,
          inputs = List(
            makeScriptTxInInfo(100),
            makePayeeTxInInfo(pkh = payees.head, idx = 0, value = 100),
            makePayeeTxInInfo(pkh = C, idx = 0, value = 50) // extra input
          ),
          outputs = List(
            (payees.head, 50 + 100 - 10),
            (payees !! 1, 50),
            (C, 50) // extra output
          ),
          fee = 10,
          expected = Left("More outputs than payees")
        )
    }

    test("failure when not all payees are payed out") {
        val payees = List(A, B)
        assertCase(
          payees,
          inputs = List(
            makeScriptTxInInfo(100),
            makePayeeTxInInfo(pkh = payees.head, idx = 0, value = 100)
          ),
          outputs = List(
            (payees.head, 50 + 100 - 10)
          ),
          fee = 10,
          expected = Left("Not all payees were paid")
        )
    }

    private val script = compile(PaymentSplitter.validator)
        .toUplc(generateErrorTraces = true)
        .plutusV3

    private val lockTxId = random[TxId]
    private val payeesTxId = random[TxId]
    private val txId = random[TxId]
    private val scriptHash = blake2b_224(ByteString.fromArray(3 +: script.cborEncoded))

    private def assertCase(
        payees: List[ByteString],
        inputs: List[TxInInfo],
        outputs: List[(ByteString, BigInt)],
        fee: BigInt,
        expected: Either[String, ExBudget]
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
            case Right(budget) =>
                assert(
                  result.isSuccess,
                  clue = s"Expected success with budget: $budget, but got: ${result.toString}"
                )
                if budget != ExBudget(ExCPU(0), ExMemory(0))
                then // Check if budget verification is requested
                    assert(
                      result.budget == budget,
                      clue = s"Expected budget: $budget, but got: ${result.budget}"
                    )
    }

    private def makePayeeTxInInfo(pkh: ByteString, idx: Int, value: BigInt): TxInInfo = {
        TxInInfo(
          outRef = TxOutRef(payeesTxId, idx),
          resolved = TxOut(
            address = Address(PubKeyCredential(PubKeyHash(pkh)), Option.None),
            value = Value.lovelace(value)
          )
        )
    }

    private def makeScriptTxInInfo(value: BigInt): TxInInfo = {
        TxInInfo(
          outRef = TxOutRef(lockTxId, 0),
          resolved = TxOut(
            address = Address(ScriptCredential(scriptHash), Option.None),
            value = Value.lovelace(value)
          )
        )
    }
}
