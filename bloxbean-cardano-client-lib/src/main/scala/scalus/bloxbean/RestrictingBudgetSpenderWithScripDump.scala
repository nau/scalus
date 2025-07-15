package scalus.bloxbean

import scalus.uplc.eval.*

import java.nio.file.Files

private[scalus] class RestrictingBudgetSpenderWithScripDump(
    val maxBudget: ExBudget,
    debugDumpFilesForTesting: Boolean
) extends BudgetSpender {
    private var cpuLeft: Long = maxBudget.cpu
    private var memoryLeft: Long = maxBudget.memory

    def spendBudget(cat: ExBudgetCategory, budget: ExBudget, env: CekValEnv): Unit = {
        if debugDumpFilesForTesting then
            cat match
                case ExBudgetCategory.BuiltinApp(fun) =>
                    Files.write(
                      java.nio.file.Paths.get("scalus.log"),
                      s"fun $$${fun}, cost: ExBudget { mem: ${budget.memory}, cpu: ${budget.cpu} }\n".getBytes,
                      java.nio.file.StandardOpenOption.CREATE,
                      java.nio.file.StandardOpenOption.APPEND
                    )
                case _ =>
        cpuLeft -= budget.cpu
        memoryLeft -= budget.memory
        if cpuLeft < 0 || memoryLeft < 0 then throw new OutOfExBudgetError(maxBudget, env)
    }

    def getSpentBudget: ExBudget =
        ExBudget.fromCpuAndMemory(maxBudget.cpu - cpuLeft, maxBudget.memory - memoryLeft)

    def reset(): Unit = {
        cpuLeft = maxBudget.cpu
        memoryLeft = maxBudget.memory
    }
}
