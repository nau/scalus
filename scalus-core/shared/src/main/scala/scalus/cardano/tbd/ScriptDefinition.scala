package scalus.cardano.tbd

import scalus.Compiler
import scalus.cardano.ledger.{Language, PlutusScript, Script}
import scalus.cardano.plutus.contract.blueprint.{mkBlueprint, mkPreamble, Blueprint, Preamble}
import scalus.uplc.{DeBruijnedProgram, Program, Term}
import scalus.sir.SIR
import scalus.prelude.Validator
import scalus.builtin.ByteString
import scalus.*

class Application(contracts: Seq[CompiledContract], title: String, description: String) {
    def blueprint: Blueprint = {
        val preamble = mkPreamble(
          title,
          description,
          version = ??? // the way types are laid out, the application allows to have 2 validators compiled to, e.g., Plutus 2 and Plutus 3
        )
        contracts.foldLeft(Blueprint(preamble))(_.addValidator(_))
    }
}

trait CompiledContract {
    def source: Validator
    def compilerOptions: Compiler.Options

    def asProgram: Program
    def asScript: PlutusScript

    class PlutusV3(override val source: Validator, override val compilerOptions: Compiler.Options)
        extends CompiledContract {
        private inline val uplc: Term =
            Compiler
                .compileInline(source.validate)
                .toUplcOptimized(using compilerOptions)()

        override def asProgram: Program = uplc.plutusV3
        override def asScript: PlutusScript = Script.PlutusV3(asProgram.cborByteString)
    }

    object PlutusV3 {
        def apply(v: Validator): PlutusV3 = {
            val options = summon[Compiler.Options]
            new PlutusV3(v, options)
        }
    }
}
