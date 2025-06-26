package scalus.sir

import scala.collection.mutable.Map as MutableMap

import scalus.sir.lowering.*
import scalus.uplc.*

class SirToUplcV3Lowering(
    sir: SIR,
    generateErrorTraces: Boolean = false,
    upcastTo: SIRType = SIRType.FreeUnificator,
    representation: LoweredValueRepresentation = TypeVarRepresentation(true),
    debug: Boolean = false
) {

    def lower(): Term = {
        val lctx = LoweringContext(
          zCombinatorNeeded = false,
          decls = MutableMap.empty,
          plutusVersion = 3,
          generateErrorTraces = generateErrorTraces,
          debug = debug
        )
        try
            given LoweringContext = lctx
            val v0 = Lowering.lowerSIR(sir)
            // transfer to default term representation
            // println(s"v representation: ${v.representation} of type ${v.sirType.show} ")
            val v1 =
                if upcastTo != SIRType.FreeUnificator then v0.upcastOne(upcastTo, v0.pos)
                else v0
            val retV = v1.toRepresentation(
              if representation == TypeVarRepresentation(true) then
                  lctx.typeGenerator(v1.sirType).defaultRepresentation(v1.sirType)
              else representation,
              v1.pos
            )
            val gctx = TermGenerationContext(
              generatedVars = Set.empty
            )
            val term = retV.termWithNeededVars(gctx)
            if lctx.zCombinatorNeeded then
                Term.Apply(Term.LamAbs("__z_combinator__", term), ExprBuilder.ZTerm)
            else term
        catch
            case e: LoweringException =>
                throw e
    }

}
