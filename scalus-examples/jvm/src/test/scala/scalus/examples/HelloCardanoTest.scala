package scalus.examples

import scalus.*
import scalus.Compiler.compile
import scalus.builtin.ByteString.*
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.ledger.api.v1.PubKeyHash
import scalus.prelude.*
import scalus.uplc.*
import scalus.uplc.eval.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.testkit.ScalusTest

import java.nio.file.{Files, Path}
import scala.language.implicitConversions

class HelloCardanoTest extends AnyFunSuite with ScalusTest {

    test("Hello Cardano") {
        val ownerPubKey = PubKeyHash(hex"1234567890abcdef1234567890abcdef1234567890abcdef12345678")
        val message = "Hello, World!".toData
        val context = makeSpendingScriptContext(
          datum = ownerPubKey.toData,
          redeemer = message,
          signatories = List(ownerPubKey)
        )

        val scalusBudget = ExBudget(ExCPU(61_329752L), ExMemory(233876L))
        val sir = compile(HelloCardano.validate)
        val result = sir.runScript(context)
        assert(result.isSuccess)
        assert(result.budget == scalusBudget)

        compareBudgetWithReferenceValue(
          testName = "HelloCardanoTest.Hello Cardano",
          scalusBudget = scalusBudget,
          refBudget = ExBudget(ExCPU(9375627L), ExMemory(28554L)),
          isPrintComparison = false
        )
    }
}
