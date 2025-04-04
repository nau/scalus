package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.ByteString.*
import scalus.builtin.Data.toData
import scalus.builtin.ToDataInstances.given
import scalus.builtin.{ByteString, Data}
import scalus.ledger.api.v3.ToDataInstances.given
import scalus.prelude.*
import scalus.uplc.*
import scalus.uplc.eval.*

class PaymentSplitterSpec extends AnyFunSuite with ScalusTest {
    test("success when payments are correctly split") {
        val context = makeSpendingScriptContext(
          datum = Data.unit,
          redeemer = Data.unit,
          signatories = List.Nil
        ).toData

        val payees = List(
          List(
            hex"1234567890abcdef1234567890abcdef1234567890abcdef12345678",
            hex"2234567890abcdef1234567890abcdef1234567890abcdef12345678",
            hex"3234567890abcdef1234567890abcdef1234567890abcdef12345678"
          )
        ).toData
//      uncomment for debugging
//        PaymentSplitter.validator(payees)(context)

        val program = compile(PaymentSplitter.validator).toUplc().plutusV3 $ payees $ context

        val result = program.evaluateDebug

        assert(result.isSuccess, clue = result.toString)
        assert(result.budget == ExBudget(ExCPU(4144100), ExMemory(26000)))
    }
}
