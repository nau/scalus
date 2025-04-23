package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.ByteString.*
import scalus.builtin.Data.toData
import scalus.builtin.ToDataInstances.given
import scalus.builtin.{ByteString, Data}
import scalus.ledger.api.v1.PubKeyHash
import scalus.ledger.api.v3.given
import scalus.prelude.*
import scalus.testkit.*
import scalus.uplc.*
import scalus.uplc.eval.*

import scala.language.implicitConversions
import scala.math.Ordering.Implicits.*

class HelloCardanoSpec extends AnyFunSuite, ScalusTest {

    test("Hello Cardano") {
        val ownerPubKey = PubKeyHash(hex"1234567890abcdef1234567890abcdef1234567890abcdef12345678")
        val message = "Hello, Cardano!".toData
        val context = makeSpendingScriptContext(
          datum = ownerPubKey.toData,
          redeemer = message,
          signatories = List(ownerPubKey)
        )

        val result = compile(HelloCardano.validate).runScript(context)

        assert(result.isSuccess)
        assert(result.budget <= ExBudget(ExCPU(62035038), ExMemory(236578)))
    }
}
