package scalus.sir.lowering.typegens

import scalus.sir.*
import scalus.sir.lowering.*

object SumCaseUplcOnlySirTypeGenerator extends SirTypeUplcGenerator {

    override def defaultRepresentation: LoweredValueRepresentation =
        SumCaseClassRepresentation.UplcConstr

    override def defaultDataRepresentation: LoweredValueRepresentation =
        SumCaseClassRepresentation.UplcConstr

    override def toRepresentation(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        SumCaseSirTypeGenerator.toRepresentation(input, representation, pos)
    }

    override def genConstr(constr: SIR.Constr)(using lctx: LoweringContext): LoweredValue = {
        throw LoweringException(
          s"SumCaseUplcOnlySirTypeGenerator does not support constructors, got ${constr.name}",
          constr.anns.pos
        )
    }

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        throw LoweringException(
          s"SumCaseUplcOnlySirTypeGenerator does not support select",
          sel.anns.pos
        )
    }

    override def genMatch(matchData: SIR.Match, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        ???
    }

}
