package scalus.sir

import scala.collection.mutable.Map as MutableMap

import scalus.sir.lowering.*
import scalus.uplc.*

class SirToUplcV3Lowering(sir: SIR, generateErrorTraces: Boolean = false, debug: Boolean = false) {

    def lower(): Term = {
        val lctx = LoweringContext(
          zCombinatorNeeded = false,
          decls = MutableMap.empty,
          plutusVersion = 3,
          generateErrorTraces = generateErrorTraces,
          debug = debug
        )
        try
            val v = Lowering.lowerSIR(sir)(using lctx)
            val gctx = TermGenerationContext(
              generatedVars = Set.empty
            )
            val term = v.termWithNeededVars(gctx)
            if lctx.zCombinatorNeeded then
                Term.Apply(Term.LamAbs("__z_combinator__", term), ExprBuilder.ZTerm)
            else term
        catch
            case e: LoweringException =>
                throw e
    }

}
