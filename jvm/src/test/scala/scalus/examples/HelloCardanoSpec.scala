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

class HelloCardanoSpec extends AnyFunSuite with ScalusTest {
    test("Hello Cardano") {
        val ownerPubKey =
            PubKeyHash(hex"1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef")
        val message = "Hello, Cardano!".toData
        val context = makeSpendingScriptContext(
          datum = ownerPubKey.toData,
          redeemer = message,
          signatories = List(ownerPubKey)
        )

        val result = compile(HelloCardano.validator).runScript(context)

        assert(result.isSuccess)
        assert(result.budget == ExBudget(ExCPU(49667700), ExMemory(187091)))
    }
}
