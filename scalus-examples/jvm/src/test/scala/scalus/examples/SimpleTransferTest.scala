package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.ByteString.*
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.ledger.api.v1.PubKeyHash
import scalus.prelude.*
import scalus.testkit.ScalusTest
import scalus.uplc.*
import scalus.uplc.eval.*

import scala.language.implicitConversions

class SimpleTransferTest extends AnyFunSuite with ScalusTest {

    inline given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

    test("Simple transfer") {
        val owner = PubKeyHash(hex"1234567890abcdef1234567890abcdef1234567890abcdef12345678")
        val receiver = PubKeyHash(hex"9234567890abcdef1234567890abcdef1234567890abcdef12345678")
        val context = makeSpendingScriptContext(
          datum = owner.toData,
          redeemer = receiver.toData,
          signatories = List(owner, receiver)
        )

        val compilerOptions = summon[Compiler.Options]
        val scalusBudget = ExBudget(ExCPU(16188743), ExMemory(58816))

        val sir = compile(SimpleTransfer.validate)

        val lw = sir.toLoweredValue()
        println("lw: " + lw.show)

        val result = sir.runScript(context)
        assert(result.isSuccess)
        assert(result.budget == scalusBudget)

        compareBudgetWithReferenceValue(
          testName = "SimpleTransfer.SimpleTransfer",
          scalusBudget = scalusBudget,
          refBudget = ExBudget(ExCPU(9375627L), ExMemory(28554L)),
          isPrintComparison = false
        )
    }
}
