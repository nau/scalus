package scalus.examples

import scalus.*
import scalus.cardano.ledger.{PlutusScript, Script, ScriptHash}
import scalus.sir.SIR
import scalus.uplc.Program

case class Config(
    generateErrorTraces: Boolean = true,
    optimizeUplc: Boolean = false,
    debug: Boolean = false
)
case class PV3Wrapper(sir: SIR, config: Config, title: String = "Plutus V3 Wrapper") {
    def toProgram: Program = {
        sir.toUplc(
          generateErrorTraces = config.generateErrorTraces,
          backend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
          optimizeUplc = config.optimizeUplc,
          debug = config.debug
        ).plutusV3
    }
    def blueprint: String = ???

    def script: PlutusScript = Script.PlutusV3(toProgram.cborByteString)
    def scriptHash: ScriptHash = script.scriptHash
}

trait Project {
    def blueprint: String
}
object BP {
    def projectOf(w: PV3Wrapper*): Project = ???
}

object PlutusV3 {
    inline def fromCode(inline code: Any): PV3Wrapper = ???
}

trait WhereWeMakeADecision {

//    def v1 = ???
//    def v2 = ???
//    def v3 = PV3Wrapper()
}
