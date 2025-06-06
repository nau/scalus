package scalus.sir.lowering.typegens

import scalus.sir.*
import scalus.sir.lowering.*

/** throw error on any attempt.
  */
object TypeNothingSirTypeGenerator extends SirTypeUplcGenerator {

    override def defaultRepresentation: LoweredValueRepresentation =
        ErrorRepresentation

    override def defaultDataRepresentation: LoweredValueRepresentation =
        throw IllegalStateException(
          "Can't ask defaultDataRepresentation access Nothing type generator"
        )

    override def toRepresentation(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue =
        input

    override def genConstr(constr: SIR.Constr)(using LoweringContext): LoweredValue = {
        throw LoweringException(
          s"TypeNothingSirTypeGenerator does not support constructors, got ${constr.name}",
          constr.anns.pos
        )
    }

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        throw LoweringException(
          s"TypeNothingSirTypeGenerator does not support select",
          sel.anns.pos
        )
    }

    override def genMatch(matchData: SIR.Match, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        throw LoweringException(
          s"TypeNothingSirTypeGenerator does not support match",
          matchData.anns.pos
        )
    }

}
