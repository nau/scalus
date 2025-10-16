package scalus.cardano.blueprint

import scalus.*
import scalus.cardano.ledger.{Language, PlutusScript, Script}
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
}

object Application {
    def apply(
        title: String,
        description: String,
        version: String,
        contracts: Seq[CompiledContract]
    ): Application = {
        val preamble = Preamble(title, description, Language.PlutusV3)
        new Application(preamble, contracts)
    }

    inline def ofSingleValidator[D, R](
        title: String,
        description: String,
        version: String,
        inline code: Any
    ): Application = {
        val contract = PlutusV3.create[D, R](title)(code)
        Application(title, description, version, Seq(contract))
    }
}

/** A smart contract compiled with Scalus. */
trait CompiledContract [R, D] {
    type Redeemer = MintRedeemer | SpendRedeemer | RewardRedeemer | CertRedeemer | VoteRedeemer | ProposeRede

    def asProgram: Program

    def asScript: PlutusScript

    def describeValidator: Validator

    def language: Language

    def sir: SIR

    def debugUplc

    def releaseUplc

    def uplcWithoutTrace
}

case class PlutusV3(
    title: String,
    description: String,
    sir: SIR,
    datumSchema: Option[PlutusDataSchema],
    redeemerSchema: Option[PlutusDataSchema]
) extends CompiledContract {
    private val uplc: Term = sir.toUplcOptimized()

    def describeValidator: Validator = {
        Validator(
          title = title,
          datum = datumSchema.map(schema => TypeDescription(schema = schema)),
          redeemer = redeemerSchema.map(schema => TypeDescription(schema = schema)),
          compiledCode = Some(asScript.script.toHex),
          hash = Some(asScript.scriptHash.toHex)
        )
    }




}


case class ContractInfo(
    def title: String
    def description: String
)


trait HtlcApp {

    def lock()
    def ulock(): Unit = {

    }
}


class MyApplication(
                       version: String,
                       validator: Validator
                   ) {
    def getBlueprint() = ???

    def foobar(plutusVersion, debug/release): CompiledContract
}

class CompiledContract {
    def uplc
    def program
    def script: PlutusScript // v1/v2/v3
})


trait Asdf {


    def asProgram: Program = uplc.plutusV3

    def asScript: Script.PlutusV3 = Script.PlutusV3(asProgram.cborByteString)

    def toUplc(using Compiler.Options): Term = sir.toUplc()

    val debugUplc: Term = sir.toUplc(generateErrorTraces = true)
    val releaseUplc: Term = sir.toUplc(generateErrorTraces = false)

    def evaluate
}

object PlutusV3 {

    inline def create[D, R](
        title: String,
        description: String = ""
    )(inline code: Any): PlutusV3 = {
        val sir = Compiler.compileInline(code)
        val datumSchema = PlutusDataSchema.derived[D]
        val redeemerSchema = PlutusDataSchema.derived[R]
        PlutusV3(title, description, sir, datumSchema, redeemerSchema)
    }

    inline def create[D, R](
                               title: String,
                               description: String = ""
                           )(inline code: v3.ScriptContext => Unit): PlutusV3 = {
        val sir = Compiler.compileInline(code)
        val datumSchema = PlutusDataSchema.derived[D]
        val redeemerSchema = PlutusDataSchema.derived[R]
        PlutusV3(title, description, sir, datumSchema, redeemerSchema)
    }

    inline def create[D, R](
                               title: String,
                               description: String = ""
                           )(inline code: scalus.prelude.Validator): PlutusV3 = {
        val sir = Compiler.compileInline(code)
        val datumSchema = PlutusDataSchema.derived[D]
        val redeemerSchema = PlutusDataSchema.derived[R]
        PlutusV3(title, description, sir, datumSchema, redeemerSchema)
    }
}
