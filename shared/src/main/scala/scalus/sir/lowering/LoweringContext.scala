package scalus.sir.lowering

import scalus.sir.*
import scalus.uplc.*

import scala.collection.mutable.Map as MutableMap

class LoweringContext(
    var zCombinatorNeeded: Boolean = false,
    val decls: MutableMap[String, DataDecl] = MutableMap.empty,
    var idSeq: Int = 0,
    var scope: LocalScope = LocalScope.empty,
    val plutusVersion: Int = 3,
    val generateErrorTraces: Boolean = false,
    val lowerFun: (SIR) => LoweredValue = (sir, scope, ctx) =>
        Lowering.lowerSIR(sir)(using ctx),
    val uplcGeneratorPolicy: SIRType => SIRTypeUplcGenerator = SIRTypeUplcGenerator(_),
) {

    def uniqueVarName(prefix: String = "_v"): String = {
        idSeq += 1
        s"$prefix$idSeq"
    }

    def lower(sir: SIR): LoweredValue = {
        lowerFun(sir, this)
    }

}
