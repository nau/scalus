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

sealed trait ScriptHeader {
    def asProgram: Program
    def asScript: PlutusScript
    def blueprint(title: String, description: String): Blueprint
}

case class PlutusV1Header private (
    source: ScriptSource,
    metadata: ScriptMetadata
) extends ScriptHeader {
    
    def asProgram: Program = source match {
        case ScriptSource.Code(sir, options) => sir.toUplc(using options).plutusV1
        case ScriptSource.Cbor(program) => program
    }
    
    def asScript: PlutusScript = PlutusV1(asProgram.cborByteString)
    
    def blueprint(title: String, description: String): Blueprint = 
        mkBlueprint(title, description, asScript)
}

case class PlutusV2Header private (
    source: ScriptSource,
    metadata: ScriptMetadata
) extends ScriptHeader {
    
    def asProgram: Program = source match {
        case ScriptSource.Code(sir, options) => sir.toUplc(using options).plutusV2
        case ScriptSource.Cbor(program) => program
    }
    
    def asScript: PlutusScript = PlutusV2(asProgram.cborByteString)
    
    def blueprint(title: String, description: String): Blueprint = 
        mkBlueprint(title, description, asScript)
}

case class PlutusV3Header private (
    source: ScriptSource,
    metadata: ScriptMetadata
) extends ScriptHeader {
    
    def asProgram: Program = source match {
        case ScriptSource.Code(sir, options) => sir.toUplc(using options).plutusV3
        case ScriptSource.Cbor(program) => program
    }
    
    def asScript: PlutusScript = PlutusV3(asProgram.cborByteString)
    
    def blueprint(title: String, description: String): Blueprint = 
        mkBlueprint(title, description, asScript)
}

enum ScriptSource {
    case Code(sir: SIR, compilerOptions: Compiler.Options)
    case Cbor(program: Program)
}

trait SourceOptions

case class ScriptMetadata(
    compilerOptions: Option[Compiler.Options],
    sourceOptions: Option[SourceOptions] = None
)

object PlutusV1Header {
    inline def code(inline code: Any)(using options: Compiler.Options): PlutusV1Header = {
        val sir = Compiler.compileInline(code)
        PlutusV1Header(
            ScriptSource.Code(sir, options),
            ScriptMetadata(Some(options))
        )
    }
    
    def validator(validator: Validator)(using options: Compiler.Options): PlutusV1Header = {
        val sir = Compiler.compileInline(validator.validate)
        PlutusV1Header(
            ScriptSource.ValidatorCode(sir, options),
            ScriptMetadata(Some(options))
        )
    }
    
    def cbored(bytes: Array[Byte]): PlutusV1Header = {
        val program = DeBruijnedProgram.fromCbor(bytes).toProgram
        PlutusV1Header(
            ScriptSource.Cbor(program),
            ScriptMetadata(None)
        )
    }
}

object PlutusV2Header {
    inline def code(inline code: Any)(using options: Compiler.Options): PlutusV2Header = {
        val sir = Compiler.compileInline(code)
        PlutusV2Header(
            ScriptSource.Code(sir, options),
            ScriptMetadata(Some(options))
        )
    }
    
    def validator(validator: Validator)(using options: Compiler.Options): PlutusV2Header = {
        val sir = Compiler.compileInline(validator.validate)
        PlutusV2Header(
            ScriptSource.ValidatorCode(sir, options),
            ScriptMetadata(Some(options))
        )
    }
    
    def cbored(bytes: Array[Byte]): PlutusV2Header = {
        val program = DeBruijnedProgram.fromCbor(bytes).toProgram
        PlutusV2Header(
            ScriptSource.Cbor(program),
            ScriptMetadata(None)
        )
    }
}

object PlutusV3Header {
    inline def code(inline code: Any)(using options: Compiler.Options): PlutusV3Header = {
        val sir = Compiler.compileInline(code)
        PlutusV3Header(
            ScriptSource.Code(sir, options),
            ScriptMetadata(Some(options))
        )
    }
    
    def validator(validator: Validator)(using options: Compiler.Options): PlutusV3Header = {
        val sir = Compiler.compileInline(validator.validate)
        PlutusV3Header(
            ScriptSource.ValidatorCode(sir, options),
            ScriptMetadata(Some(options))
        )
    }
    
    def cbored(bytes: Array[Byte]): PlutusV3Header = {
        val program = DeBruijnedProgram.fromCbor(bytes).toProgram
        PlutusV3Header(
            ScriptSource.Cbor(program),
            ScriptMetadata(None)
        )
    }
}

object ScriptHeader {
    object v1 {
        inline def code(inline code: Any)(using options: Compiler.Options): PlutusV1Header = 
            PlutusV1Header.code(code)
        
        def validator(validator: Validator)(using options: Compiler.Options): PlutusV1Header = 
            PlutusV1Header.validator(validator)
        
        def cbored(bytes: Array[Byte]): PlutusV1Header = 
            PlutusV1Header.cbored(bytes)
    }
    
    object v2 {
        inline def code(inline code: Any)(using options: Compiler.Options): PlutusV2Header = 
            PlutusV2Header.code(code)
        
        def validator(validator: Validator)(using options: Compiler.Options): PlutusV2Header = 
            PlutusV2Header.validator(validator)
        
        def cbored(bytes: Array[Byte]): PlutusV2Header = 
            PlutusV2Header.cbored(bytes)
    }
    
    object v3 {
        inline def code(inline code: Any)(using options: Compiler.Options): PlutusV3Header = 
            PlutusV3Header.code(code)
        
        def validator(validator: Validator)(using options: Compiler.Options): PlutusV3Header = 
            PlutusV3Header.validator(validator)
        
        def cbored(bytes: Array[Byte]): PlutusV3Header = 
            PlutusV3Header.cbored(bytes)
    }
}
