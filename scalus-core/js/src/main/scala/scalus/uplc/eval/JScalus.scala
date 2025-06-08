package scalus.uplc.eval

import scalus.builtin.given
import scalus.uplc.DeBruijnedProgram

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("Scalus")
object JScalus {

    extension (self: ExBudget)
        /** Converts ExBudget to a JavaScript BigInt representation. */
        def toJSExBudget: JSExBudget = new JSExBudget(
          cpu = js.BigInt(self.cpu.toString),
          memory = js.BigInt(self.memory.toString)
        )

    @JSExportTopLevel("ExBudget")
    class JSExBudget(val cpu: js.BigInt, val memory: js.BigInt) extends js.Object

    @JSExportTopLevel("Result")
    class JSResult(val isSuccess: Boolean, val budget: JSExBudget, val logs: js.Array[String])
        extends js.Object

    @JSExport
    def evaluateScript(scriptCborHex: String): JSResult = {
        try
            // Parse script from hex
            val program = DeBruijnedProgram.fromCborHex(scriptCborHex)
            // Create appropriate VM based on version
            val vm = PlutusVM.makePlutusV3VM()
            // Evaluate script
            vm.evaluateScriptDebug(program) match
                case Result.Success(term, budget, costs, logs) =>
                    JSResult(
                      isSuccess = true,
                      budget = budget.toJSExBudget,
                      logs = js.Array(logs*)
                    )
                case Result.Failure(exception, budget, costs, logs) =>
                    JSResult(
                      isSuccess = false,
                      budget = budget.toJSExBudget,
                      logs = js.Array(logs*)
                    )
        catch
            case exception: Exception =>
                JSResult(
                  isSuccess = false,
                  budget = ExBudget.zero.toJSExBudget,
                  logs = js.Array(exception.getMessage)
                )
    }
}
