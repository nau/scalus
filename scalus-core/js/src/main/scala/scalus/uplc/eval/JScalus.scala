package scalus.uplc.eval

import scalus.builtin.{Data, given}
import scalus.uplc.{Constant, DeBruijnedProgram, Term}

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

    /** Applies a data argument to a Plutus script given its CBOR hex representation.
      *
      * @param doubleCborHex
      *   The double-CBOR-encoded hex representation of the Plutus script.
      * @param data
      *   The JSON representation of the [[Data]] argument to apply.
      * @return
      *   The double-CBOR-encoded hex representation of the script with the data argument applied.
      */
    @JSExport
    def applyDataArgToScript(doubleCborHex: String, data: String): String = {
        // Parse script and data from hex
        val program = DeBruijnedProgram.fromDoubleCborHex(doubleCborHex)
        val arg = Data.fromJson(data)
        val applied = program $ Term.Const(Constant.Data(arg))
        applied.doubleCborHex
    }

    /** Evaluates a Plutus script with the given CBOR hex representation.
      *
      * @param doubleCborHex
      *   The double-CBOR-encoded hex representation of the Plutus script.
      * @return
      *   A JSResult containing the evaluation result, budget, and logs.
      */
    @JSExport
    def evaluateScript(doubleCborHex: String): JSResult = {
        try
            // Parse script from hex
            val program = DeBruijnedProgram.fromDoubleCborHex(doubleCborHex)
            // Create appropriate VM based on version
            val vm = PlutusVM.makePlutusV3VM()
            // Evaluate script
            vm.evaluateScriptDebug(program) match
                case Result.Success(_, budget, _, logs) =>
                    JSResult(
                      isSuccess = true,
                      budget = budget.toJSExBudget,
                      logs = js.Array(logs*)
                    )
                case Result.Failure(exception, budget, _, logs) =>
                    JSResult(
                      isSuccess = false,
                      budget = budget.toJSExBudget,
                      logs = js.Array(exception.getMessage +: logs*)
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
