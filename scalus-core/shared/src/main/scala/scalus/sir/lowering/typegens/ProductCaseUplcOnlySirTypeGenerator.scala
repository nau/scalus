package scalus.sir.lowering.typegens

import scalus.sir.*
import scalus.sir.lowering.*

object ProductCaseUplcOnlySirTypeGenerator extends SirTypeUplcGenerator {

    override def defaultRepresentation: LoweredValueRepresentation = ???

    override def defaultDataRepresentation: LoweredValueRepresentation =
        throw IllegalStateException(
          "Can't ask defaultDataRepresentaion access product case type generator"
        )

    override def toRepresentation(
        input: LoweredValue,
        outputRepresentation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        ???
    }

    override def genConstr(constr: SIR.Constr)(using LoweringContext): LoweredValue = ???

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        LoweringContext
    ): LoweredValue = {
        ???
    }

    override def genMatch(matchData: SIR.Match, loweredScrutinee: LoweredValue)(using
        LoweringContext
    ): LoweredValue = {
        ???
    }

}
