package scalus.sir.lowering.typegens

import scalus.sir.*
import scalus.sir.lowering.*
import scalus.uplc.*

class ProductCaseSirTypeGenerator extends SIRTypeUplcGenerator {

    override def defaultRepresentation: LoweredValueRepresentation =
        ProductCaseClassRepresentation.DataList

    override def defaultDataRepresentation: LoweredValueRepresentation =
        ProductCaseClassRepresentation.PackedDataList

    override def toRepresentation(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        ???
    }

    override def genConstr(constr: SIR.Constr)(using lctx: LoweringContext): LoweredValue = {
        val loweredArgs = constr.args.map(arg => lctx.lower(arg))
        ???
    }

    override def genSelect(sel: SIR.Select)(using LoweringContext): LoweredValue = ???

    override def genMatch(matchData: SIR.Match, loweredScrutinee: LoweredValue)(using
        LoweringContext
    ): LoweredValue = {
        ???
    }

}
