package scalus.uplc.eval

import scalus.uplc.eval.*

import java.nio.file.Files

class RestrictingBudgetSpenderWithScriptDump(
    maxBudget: ExBudget,
    debugDumpFilesForTesting: Boolean
) extends RestrictingBudgetSpender(maxBudget) {
    override def spendBudget(cat: ExBudgetCategory, budget: ExBudget, env: CekValEnv): Unit = {
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
        super.spendBudget(cat, budget, env)
    }
}
