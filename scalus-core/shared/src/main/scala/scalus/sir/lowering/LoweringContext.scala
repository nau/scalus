package scalus.sir.lowering

import scalus.sir.*
import scalus.sir.lowering.typegens.SirTypeUplcGenerator
import scala.collection.mutable.Map as MutableMap

class LoweringContext(
                         var zCombinatorNeeded: Boolean = false,
                         val decls: MutableMap[String, DataDecl] = MutableMap.empty,
                         var varIdSeq: Int = 0,
                         var scope: LocalScope = LocalScope.empty,
                         val plutusVersion: Int = 3,
                         val generateErrorTraces: Boolean = false,
                         val uplcGeneratorPolicy: SIRType => SirTypeUplcGenerator = SirTypeUplcGenerator(_),
                         val typeVars: Map[SIRType.TypeVar, SIRType] = Map.empty,
) {

    def uniqueVarName(prefix: String = "_v"): String = {
        varIdSeq += 1
        s"$prefix$varIdSeq"
    }

    def lower(sir: SIR): LoweredValue = {
        Lowering.lowerSIR(sir)(using this)
    }

    def typeGenerator(sirType: SIRType): SirTypeUplcGenerator = {
        uplcGeneratorPolicy(sirType)
    }

}
