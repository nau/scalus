package scalus.sir.lowering

import scalus.sir.*
import scalus.uplc.*

import scala.collection.mutable.{Map => MutableMap}

class LoweringContext(
    var zCombinatorNeeded: Boolean = false,
    val decls: MutableMap[String, DataDecl] = MutableMap.empty,
    val plutusVersion: Int = 3,
    val generateErrorTraces: Boolean = false,
    var idSeq: Int = 0,
    val lowerFun: (SIR, LoweringContext) => LoweredValue = (sir, ctx) =>
        Lowering.lowerSIR(sir)(using ctx)
) {

    def uniqueVarName(prefix: String = "_v"): String = {
        idSeq += 1
        s"$prefix$idSeq"
    }

    def lower(sir: SIR): LoweredValue = {
        lowerFun(sir, this)
    }

}
