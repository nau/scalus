package scalus.cardano.plutus.contract.blueprint

import scalus.*
import scalus.cardano.ledger.{Language, PlutusScript, Script}
import scalus.cardano.plutus.contract.blueprint
import scalus.sir.SIR
import scalus.uplc.{Program, Term}

/** A description of a Scalus application, containing one or more contracts.
  */
case class Application(
    preamble: Preamble,
    contracts: Seq[CompiledContract],
) {

    /** A CIP-57 compliant Blueprint, describing the application. */
    def blueprint: Blueprint = {
        Blueprint(preamble, validators = contracts.map(_.describeValidator))
    }

    inline def addValidator[D, R](validatorTitle: String, inline code: Any): Application = {
        copy(contracts = contracts :+ PlutusV3.create(validatorTitle, code = code))
    }
}

object Application {
    def apply(
        title: String,
        description: String,
        version: String,
        contracts: Seq[CompiledContract]
    ): Application = {
        val preamble = Blueprint.mkPreamble(title, description, Language.PlutusV3)
        new Application(preamble, contracts)
    }

    inline def ofSingleValidator[D, R](
        title: String,
        description: String,
        version: String,
        inline code: Any
    ): Application = {
        val contract = PlutusV3.create[D, R](title, code = code)
        Application(title, description, version, Seq(contract))
    }
}

/** A smart contract compiled with Scalus. */
trait CompiledContract {
    def asProgram: Program

    def asScript: PlutusScript

    def describeValidator: blueprint.Validator

    def sir: SIR
}
case class PlutusV3(
    title: String,
    description: String,
    sir: SIR,
    datumSchema: Option[PlutusDataSchema],
    redeemerSchema: Option[PlutusDataSchema]
) extends CompiledContract {
    private val uplc: Term = sir.toUplcOptimized()

    def describeValidator: blueprint.Validator = {
        blueprint.Validator(
          title = title,
          datum = datumSchema.map(schema => TypeDescription(schema = schema)),
          redeemer = redeemerSchema.map(schema => TypeDescription(schema = schema)),
          compiledCode = Some(asScript.script.toHex),
          hash = Some(asScript.scriptHash.toHex)
        )
    }
    def asProgram: Program = uplc.plutusV3

    def asScript: Script.PlutusV3 = Script.PlutusV3(asProgram.cborByteString)
}

object PlutusV3 {

    inline def create[D, R](
        title: String,
        description: String = "",
        inline code: Any,
    ): PlutusV3 = {
        val sir = Compiler.compileInline(code)
        val datumSchema = PlutusDataSchema.derived[D]
        val redeemerSchema = PlutusDataSchema.derived[R]
        PlutusV3(title, description, sir, datumSchema, redeemerSchema)
    }
}
