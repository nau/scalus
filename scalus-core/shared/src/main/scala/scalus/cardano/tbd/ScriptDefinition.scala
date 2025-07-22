package scalus.cardano.tbd

import scalus.Compiler
import scalus.cardano.ledger.{Language, PlutusScript, Script}
import scalus.cardano.plutus.contract.blueprint
import scalus.cardano.plutus.contract.blueprint.{mkBlueprint, mkPreamble, Blueprint, Preamble}
import scalus.uplc.{DeBruijnedProgram, Program, Term}
import scalus.sir.SIR
import scalus.prelude.Validator
import scalus.builtin.ByteString
import scalus.*

case class Application(
    preamble: Preamble,
    contracts: Seq[CompiledContract],
) {
    def blueprint: Blueprint = {
        val preamble = mkPreamble(
          title,
          description,
          version =
              ??? // the way types are laid out, the application allows to have 2 validators compiled to, e.g., Plutus 2 and Plutus 3
        )
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
        new Application(preamble, contracts, title, description)
    }
}

trait CompiledContract {
    def asProgram: Program

    def asScript: PlutusScript

    def bpv: blueprint.Validator

    def sir: SIR
}
class PlutusV3(val sir: SIR, val bpv: blueprint.Validator) extends CompiledContract {
    private val uplc: Term = ???
    def asProgram: Program = uplc.plutusV3
    def asScript: PlutusScript = Script.PlutusV3(asProgram.cborByteString)
}

object PlutusV3 {
    inline def apply(bpv: blueprint.Validator)(inline v: Validator): PlutusV3 = {
        new PlutusV3(Compiler.compileInline(v.validate))
    }
}

def asdf[Redeemer, Datum]() = {
    summon[PlutusDataSchema[Redeemer]]
}

object Example {
    def validator(ctx: Data): Unit = ()

    val contract = PlutusV3(
      blueprint.Validator(
        title = "ExampleValidator",
        datum = Some(summon[PlutusDataSchema[Redeemer]]),
        redeemer = Some(blueprint.TypeDescription("ExampleRedeemer"))
      )
    )(validator)

    val contract2 = PlutusV3(
      blueprint.Validator(
        title = "ExampleValidator",
        datum = Some(summon[PlutusDataSchema[Redeemer]]),
        redeemer = Some(blueprint.TypeDescription("ExampleRedeemer"))
      )
    )(validator)


    /*
    *
    * val contract2 = PlutusV3.create[MyRedeemer, MyDatum](validator)
    * */

    val application = Application(
      title = "Example Application",
      description = "An example application with multiple contracts",
      version = "1.0.0",
      contracts = Seq(contract, contract2)
    )
//    application.blueprint.saveToFile("example_blueprint.json")
    



}
