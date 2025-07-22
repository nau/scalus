package scalus.cardano.tbd

import scalus.*
import scalus.builtin.ByteString
import scalus.cardano.ledger.{Language, PlutusScript, Script}
import scalus.cardano.plutus.contract.blueprint
import scalus.cardano.plutus.contract.blueprint.Blueprint.Preamble
import scalus.cardano.plutus.contract.blueprint.{Blueprint, PlutusDataSchema, TypeDescription, mkPreamble}
import scalus.prelude.Validator
import scalus.sir.SIR
import scalus.uplc.{Program, Term}

case class Application(
    preamble: Preamble,
    contracts: Seq[CompiledContract],
) {
    def blueprint: Blueprint = {
        Blueprint(preamble, validators = contracts.map(_.bpv))
    }
}

object Application {
    def apply(
        title: String,
        description: String,
        version: String,
        contracts: Seq[CompiledContract]
    ): Application = {
        val preamble = mkPreamble(title, description, Language.PlutusV3)
        new Application(preamble, contracts)
    }
}

trait CompiledContract {
    def asProgram: Program

    def asScript: PlutusScript

    def bpv: blueprint.Validator

    def sir: SIR
}
class PlutusV3(val sir: SIR, val bpv: blueprint.Validator) extends CompiledContract {
    private val uplc: Term = sir.toUplcOptimized()
    def asProgram: Program = uplc.plutusV3
    def asScript: PlutusScript = Script.PlutusV3(asProgram.cborByteString)
}

object PlutusV3 {
    inline def create[D, R](inline v: Validator): PlutusV3 = {
        val sir = Compiler.compileInline((scData: scalus.builtin.Data) => v.validate(scData))
        val title = v.getClass.getName
        val datum = PlutusDataSchema.derived[D]
        val redeemer = PlutusDataSchema.derived[R]
        val bpv = blueprint.Validator(
          title = title,
          datum = Some(TypeDescription(schema = datum)),
          redeemer = Some(TypeDescription(schema = redeemer))
        )
        new PlutusV3(sir, bpv)
    }
}
