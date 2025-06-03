package scalus.sir

import scala.collection.mutable.Map as MutableMap

import scalus.sir.lowering.*
import scalus.uplc.*

class SirToUplcV3Lowering(sir: SIR, generateErrorTraces: Boolean = false) {

    def lower(): Term = {
        val lctx = LoweringContext(
          zCombinatorNeeded = false,
          decls = MutableMap.empty,
          plutusVersion = 3,
          generateErrorTraces = generateErrorTraces
        )
        val v = Lowering.lowerSIR(sir)(using lctx)
        val gctx = TermGenerationContext(
          generatedVars = Set.empty
        )
        v.termWithNeddedVars(gctx)
    }

}
