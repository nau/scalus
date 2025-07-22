package scalus.cardano.tbd

import scalus.Compiler
import scalus.cardano.ledger.{Language, PlutusScript}
import scalus.cardano.ledger.Script.{PlutusV1, PlutusV2, PlutusV3}
import scalus.cardano.plutus.contract.blueprint.{mkBlueprint, Blueprint}
import scalus.uplc.{DeBruijnedProgram, Program}
import scalus.sir.SIR
import scalus.prelude.Validator
import scalus.builtin.ByteString
import scalus.*

case class ScriptDefinition(
    source: ScriptSource,
    metadata: ScriptMetadata,
    plutusVersion: Language
) {
    
    def asProgram: Program = source match {
        case ScriptSource.Code(sir, options) =>
            val uplc = sir.toUplc(using options)
            plutusVersion match {
                case Language.PlutusV1 => uplc.plutusV1
                case Language.PlutusV2 => uplc.plutusV2
                case Language.PlutusV3 => uplc.plutusV3
            }
        case ScriptSource.ValidatorCode(validator, options) =>
            val sir = Compiler.compileInline(validator.validate)
            val uplc = sir.toUplc(using options)
            plutusVersion match {
                case Language.PlutusV1 => uplc.plutusV1
                case Language.PlutusV2 => uplc.plutusV2
                case Language.PlutusV3 => uplc.plutusV3
            }
        case ScriptSource.Cbor(program) => program
    }
    
    def asScript: PlutusScript = {
        val program = asProgram
        plutusVersion match {
            case Language.PlutusV1 => PlutusV1(program.cborByteString)
            case Language.PlutusV2 => PlutusV2(program.cborByteString)
            case Language.PlutusV3 => PlutusV3(program.cborByteString)
        }
    }
    def asBlueprint(title: String, description: String): Blueprint = {
        mkBlueprint(title, description, asScript)
    }
}
enum ScriptSource {
    case Code(sir: SIR, compilerOptions: Compiler.Options)
    case ValidatorCode(validator: Validator, compilerOptions: Compiler.Options)
    case Cbor(program: Program)(program: Program)
}

case class ScriptMetadata(
    compilerOptions: Option[Compiler.Options],
)

object ScriptDefinition {

    inline def code(inline code: Any, plutusVersion: Language = Language.PlutusV3)(using
        options: Compiler.Options
    ): ScriptDefinition = {
        val sir = Compiler.compileInline(code)
        val metadata = ScriptMetadata(
          compilerOptions = Some(options),
          timestamp = Some(System.currentTimeMillis())
        )

        ScriptDefinition(
          source = ScriptSource.Code(sir, options),
          metadata = metadata,
          plutusVersion = plutusVersion
        )
    }

    def validator(validator: Validator, plutusVersion: Language = Language.PlutusV3)(using
        options: Compiler.Options
    ): ScriptDefinition = {
        val metadata = ScriptMetadata(compilerOptions = Some(options))

        ScriptDefinition(
          source = ScriptSource.ValidatorCode(validator, options),
          metadata = metadata,
          plutusVersion = plutusVersion
        )
    }
    def cbored(bytes: Array[Byte]): ScriptDefinition = {
        val program = DeBruijnedProgram.fromCbor(bytes).toProgram
        val language = Language.from(program.version)
        val metadata = ScriptMetadata(
          compilerOptions = None, // Lost during compilation
        )

        ScriptDefinition(
          source = ScriptSource.Cbor(program),
          metadata = metadata,
          plutusVersion = language
        )
    }
}
