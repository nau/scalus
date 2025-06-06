package scalus.sir.lowering.typegens

import scalus.sir.*
import scalus.sir.lowering.*

/** We assume that type variable can be converted to data, and it is impossible to do something
  * meaningful with it in the UPLC, except pass the value as an argument in unchanged form.
  */
object TypeVarSirTypeGenerator extends SirTypeUplcGenerator {

    override def defaultRepresentation: LoweredValueRepresentation =
        TypeVarDataRepresentation

    override def defaultDataRepresentation: LoweredValueRepresentation =
        TypeVarDataRepresentation

    override def toRepresentation(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue =
        input

    override def genConstr(constr: SIR.Constr)(using lctx: LoweringContext): LoweredValue = {
        throw LoweringException(
          s"TypeVarSirTypeGenerator does not support constructors, got ${constr.name}",
          constr.anns.pos
        )
    }

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        throw LoweringException(
          s"TypeVarSirTypeGenerator does not support select",
          sel.anns.pos
        )
    }

    override def genMatch(matchData: SIR.Match, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        throw LoweringException(
          s"TypeVarSirTypeGenerator does not support match",
          matchData.anns.pos
        )
    }

}
