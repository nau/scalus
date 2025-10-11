package scalus.uplc.eval

import scalus.builtin.platform
import scalus.uplc.eval.*

class RestrictingBudgetSpenderWithScriptDump(
    maxBudget: ExBudget,
    debugDumpFilesForTesting: Boolean
) extends RestrictingBudgetSpender(maxBudget) {
    override def spendBudget(cat: ExBudgetCategory, budget: ExBudget, env: CekValEnv): Unit = {
        if debugDumpFilesForTesting then
            cat match
                case ExBudgetCategory.BuiltinApp(fun) =>
                    val logMessage =
                        s"fun $$${fun}, cost: ExBudget { mem: ${budget.memory}, cpu: ${budget.cpu} }\n"
                    platform.appendFile("scalus.log", logMessage.getBytes("UTF-8"))
                case _ =>
        super.spendBudget(cat, budget, env)
    }
}
