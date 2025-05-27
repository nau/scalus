package scalus.sir.lowering

import scalus.sir.*
import scalus.uplc.*

import scala.collection.mutable.Map as MutableMap

class LoweringContext(
                         var zCombinatorNeeded: Boolean = false,
                         val decls: MutableMap[String, DataDecl] = MutableMap.empty,
                         var varIdSeq: Int = 0,
                         var nodeIdSeq: Int = 0,
                         var scope: LocalScope = LocalScope.empty,
                         val plutusVersion: Int = 3,
                         val generateErrorTraces: Boolean = false,
                         val uplcGeneratorPolicy: SIRType => SIRTypeUplcGenerator = SIRTypeUplcGenerator(_),
) {


    def uniqueVarName(prefix: String = "_v"): String = {
        varIdSeq += 1
        s"$prefix$varIdSeq"
    }

    def uniqueNodeName(prefix: String = "_n"): String = {
        nodeIdSeq += 1
        s"$prefix$nodeIdSeq"
    }

    def lower(sir: SIR): LoweredValue = {
        Lowering.lowerSIR(sir)(using this)
    }

    def typeGenerator(sirType: SIRType): SIRTypeUplcGenerator = {
        uplcGeneratorPolicy(sirType)
    }

}
