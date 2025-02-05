package scalus.uplc
package eval

import io.bullet.borer.Cbor
import scalus.builtin.{NativePlatformSpecific, PlatformSpecific}
import scalus.ledger.api.{MajorProtocolVersion, PlutusLedgerLanguage}
import scalus.uplc
import scalus.utils.Utils

import scala.scalanative.unsafe.*
import scala.scalanative.runtime.ffi
import scala.scalanative.unsigned.*

/** Scala Native bindings for the UPLC evaluation library.
  */
private object LibScalus:
    private given platformSpecific: PlatformSpecific = NativePlatformSpecific

    /* struct eval_result {
     *     long long cpu;
     *     long long memory;
     *     char* logs;
     *     char* error;
     * };
     * */
    type EvalResult = CStruct4[CLongLong, CLongLong, CString, CString]

    /** Converts a double CBOR HEX encoded script to a [[Flat]] encoded UPLC program.
      * @param scriptHex
      *   The double CBOR HEX encoded script
      * @param result
      *   Pointer to the result buffer
      * @param size
      *   Size of the result buffer
      * @return
      */
    @exported(name = "scalus_flat_script_from_hex")
    def scalus_flat_script_from_hex(scriptHex: CString, result: Ptr[Ptr[Byte]], size: CSize): CInt =
        Zone {
            try
                // Parse script from hex
                val scriptBytes = Utils.hexToBytes(fromCString(scriptHex))
                val cbor = Cbor.decode(scriptBytes).to[Array[Byte]].value
                val scriptFlat = Cbor.decode(cbor).to[Array[Byte]].value
                if size < scriptFlat.length.toCSize then
                    !result = c"Buffer size is too small for the script"
                    1
                else
                    ffi.memcpy(result, scriptFlat.atUnsafe(0), scriptFlat.length.toCSize)
                    0
            catch
                case e: Exception =>
                    !result = toCString(e.getMessage)
                    1
        }

    @exported(name = "scalus_get_machine_params_from_cardano_cli_protocol_params_json")
    def fromCardanoCliProtocolParamsJson(json: CString, plutusVersion: CInt): MachineParams = {
        val jsonStr = fromCString(json)
        try
            val pll = PlutusLedgerLanguage.fromOrdinal(plutusVersion - 1)
            MachineParams.fromCardanoCliProtocolParamsJson(jsonStr, pll)
        catch case _: Exception => null
    }

    @exported(name = "scalus_get_default_machine_params")
    def defaultMachineParams(plutusVersion: CInt, protocolVersion: CInt): MachineParams = {
        try
            val pll = PlutusLedgerLanguage.fromOrdinal(plutusVersion - 1)
            MachineParams.defaultParamsFor(pll, MajorProtocolVersion(protocolVersion))
        catch
            case e: Exception =>
                println(s"Error: ${e.getMessage}")
                null
    }

    @exported(name = "scalus_evaluate_script")
    def scalus_evaluate_script(
        scriptHex: CString,
        plutusVersion: CInt,
        result: Ptr[EvalResult]
    ): CInt = Zone {
        try
            // Parse script from hex
            val program = DeBruijnedProgram.fromDoubleCborHex(fromCString(scriptHex))

            // Create appropriate VM based on version
            val vm = plutusVersion match
                case 1 => PlutusVM.makePlutusV1VM()
                case 2 => PlutusVM.makePlutusV2VM()
                case 3 => PlutusVM.makePlutusV3VM()
                case _ =>
                    throw new IllegalArgumentException(
                      s"Unsupported Plutus version: $plutusVersion"
                    )

            // Evaluate script
            vm.evaluateScriptDebug(program) match
                case Result.Success(term, budget, costs, logs) =>
                    result._1 = budget.cpu
                    result._2 = budget.memory
                    result._3 = toCString(logs.mkString("\n"))
                    result._4 = null
                    0
                case Result.Failure(exception, budget, costs, logs) =>
                    result._1 = budget.cpu
                    result._2 = budget.memory
                    result._3 = toCString(logs.mkString("\n"))
                    result._4 = toCString(exception.getMessage)
                    1
        catch
            case e: Exception =>
                result._1 = 0
                result._2 = 0
                result._3 = null
                result._4 = toCString(e.getMessage)
                2
    }
