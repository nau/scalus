package scalus.uplc
package eval

import io.bullet.borer.Cbor
import scalus.builtin.{NativePlatformSpecific, PlatformSpecific}
import scalus.utils.Utils

import scala.scalanative.unsafe.*

private object LibScalus:
    private given platformSpecific: PlatformSpecific = NativePlatformSpecific

    @exported(name = "scalus_evaluate_script")
    def scalus_evaluate_script(scriptHex: CString, plutusVersion: CInt): CInt = Zone {
        try
            // Parse script from hex
            val scriptBytes = Utils.hexToBytes(fromCString(scriptHex))
            val cbor = Cbor.decode(scriptBytes).to[Array[Byte]].value
            val scriptFlat = Cbor.decode(cbor).to[Array[Byte]].value
            val program = DeBruijnedProgram.fromFlatEncoded(scriptFlat)

            // Create appropriate VM based on version
            val vm = plutusVersion match
                case 1 => PlutusVM.makePlutusV1VM()
                case 2 => PlutusVM.makePlutusV2VM()
                case 3 => PlutusVM.makePlutusV3VM()
                case _ =>
                    throw new IllegalArgumentException(
                      s"Unsupported Plutus version: $plutusVersion"
                    )

            // Create budget spender
            val spender = CountingBudgetSpender()

            // Evaluate script
            vm.evaluateScript(program, spender, NoLogger)

            // Check budget
            0
        catch
            case e: Exception =>
                1
    }
