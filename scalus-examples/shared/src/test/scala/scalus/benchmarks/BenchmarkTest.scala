package scalus.benchmarks

import scalus.*
import scalus.uplc.*
import scalus.uplc.eval.*
import scalus.sir.SIR
import org.scalatest.funsuite.AnyFunSuite
import scalus.testkit.ScalusTest

trait BenchmarkTest extends AnyFunSuite with ScalusTest {
    def isAlwaysPrintComparison: Boolean

    extension (self: SIR)
        def check(
            testName: String,
            scalusBudget: ExBudget,
            aikenBudget: ExBudget,
            isPrintComparison: Boolean = false
        ): Unit =
            extension (scalus: Long)
                def comparisonAsJsonString(aiken: Long): String =
                    val value = aiken.toDouble / scalus.toDouble
                    val winner =
                        if value == 1 then "draw" else if value > 1 then "scalus" else "aiken"

                    s"{" +
                        s"aiken: $aiken, scalus: $scalus, " +
                        s"comparison: {value: $value, winner: $winner}" +
                        s"}"

            end extension

            val result = self.toUplcOptimized(false).evaluateDebug
            result match
                case Result.Success(Term.Const(Constant.Bool(true)), budget, _, _) =>
                    if isAlwaysPrintComparison || isPrintComparison then
                        println(
                          s"Benchmark.$testName{" +
                              s"cpu: ${budget.cpu.comparisonAsJsonString(aikenBudget.cpu)}, " +
                              s"memory: ${budget.memory.comparisonAsJsonString(aikenBudget.memory)}" +
                              "}"
                        )
                    assert(budget == scalusBudget)
                case _ => fail(s"Test $testName failed: $result")

    end extension
}
