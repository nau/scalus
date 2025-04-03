package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.ByteString.given
import scalus.builtin.Data.toData
import scalus.builtin.ToDataInstances.given
import scalus.builtin.{ByteString, Data, given}
import scalus.ledger.api.v1.{Interval, PubKeyHash}
import scalus.ledger.api.v3.ToDataInstances.given
import scalus.ledger.api.v3.{*, given}
import scalus.prelude.*
import scalus.uplc.*
import scalus.uplc.TermDSL.given
import scalus.uplc.eval.*

import scala.language.implicitConversions

class PaymentSplitterSpec extends AnyFunSuite with ScalusTest {
    test("success when payments are correctly split") {
        val message = "Hello, Cardano!".toData
        val context = makeSpendingScriptContext(
          datum = Data.unit,
          redeemer = message,
          signatories = List.Nil
        )

//        val uplc = compile(PaymentSplitter.validator).toUplc() $ context.toData
//        val result = uplc.evaluateDebug

//        assert(result.isSuccess, result.toString)
//        assert(result.budget == ExBudget(ExCPU(49667700), ExMemory(187091)))
    }
}
