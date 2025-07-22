package scalus.cardano.tbd

import scalus.Compiler
import scalus.cardano.ledger.{Language, PlutusScript, Script}
import scalus.cardano.plutus.contract.blueprint.{mkBlueprint, Blueprint}
import scalus.cardano.tbd.CompiledContract.PreCompilationState.{Code, Unknown, Validator}
import scalus.uplc.{DeBruijnedProgram, Program}
import scalus.sir.SIR
import scalus.prelude.Validator
import scalus.builtin.ByteString
import scalus.*

class Application(contracts: Seq[CompiledContract], title: String, description: String) {
    def blueprint: Blueprint = {
        contracts.map(_.source).collect {
            case Validator(value, options) =>
            // The only branch where we know datum and redeemer types without requiring the caller to supply them
                ???
            case Code(value, options) => ???
            case Unknown              => ???
        }
    }
}

trait CompiledContract {
    def source: PreCompilationState
    def compilationResult: Array[
      Byte
    ] // feels like there should be a `Cbor` wrapper, kind of like `KeepRaw` or `Hash`
    class PlutusV3(
        override val source: PreCompilationState,
        override val compilationResult: Array[Byte]
    ) extends CompiledContract {

        def asProgram: Program = DeBruijnedProgram.fromCbor(compilationResult).toProgram
        def asScript: PlutusScript = Script.PlutusV3(ByteString.fromArray(compilationResult))
    }

    object PlutusV3 {
        def apply(v: Validator): PlutusV3 = {
            val options = summon[Compiler.Options]
            val program = Compiler.compileInline(v.validate).toUplcOptimized(options)().plutusV3
            new PlutusV3(PreCompilationState.Validator(v, options), program.cborEncoded)
        }

        inline def apply(inline code: Any): PlutusV3 = {
            val options = summon[Compiler.Options]
            val program = Compiler.compileInline(code).toUplcOptimized(options)().plutusV3
            new PlutusV3(PreCompilationState.Code(code, options), program.cborEncoded)
        }

        def fromCbor(bytes: Array[Byte]): PlutusV3 = new PlutusV3(
          PreCompilationState.Unknown,
          bytes
        )
    }

    enum PreCompilationState {
        case Code(value: Any, options: Compiler.Options)
        case Validator(value: scalus.prelude.Validator, options: Compiler.Options)
        case Unknown
    }
}
