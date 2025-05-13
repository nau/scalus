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

            self match
                case Result.Success(_, budget, _, _) =>
                    if isPrintComparison then
                        println(
                          s"$testName: {" +
                              s"cpu: ${budget.cpu.comparisonAsJsonString(aikenBudget.cpu)}, " +
                              s"memory: ${budget.memory.comparisonAsJsonString(aikenBudget.memory)}" +
                              "}"
                        )
                    assert(budget == scalusBudget)
                case _ => fail(s"Test $testName failed: $self")

    end extension
}
