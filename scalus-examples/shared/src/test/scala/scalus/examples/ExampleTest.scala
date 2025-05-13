package scalus.examples

import scalus.*
import scalus.uplc.*
import scalus.uplc.eval.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.testkit.ScalusTest

class ExampleTest extends AnyFunSuite with ScalusTest {
    extension (self: Result)
        def check(
            testName: String,
            scalusBudget: ExBudget,
            refBudget: ExBudget,
            isPrintComparison: Boolean = false
        ): Unit =
            extension (scalus: Long)
                def comparisonAsJsonString(ref: Long): String = {
                    val comparison = f"${scalus.toDouble / ref.toDouble * 100}%.2f"
                    s"{scalus: $scalus, ref: $ref, comparison: $comparison%}"
                }

            end extension

            self match
                case Result.Success(_, budget, _, _) =>
                    if isPrintComparison then
                        println(
                          s"$testName: {" +
                              s"cpu: ${budget.cpu.comparisonAsJsonString(refBudget.cpu)}, " +
                              s"memory: ${budget.memory.comparisonAsJsonString(refBudget.memory)}" +
                              "}"
                        )
                    assert(budget == scalusBudget)
                case _ => fail(s"Test $testName failed: $self")

    end extension
}
