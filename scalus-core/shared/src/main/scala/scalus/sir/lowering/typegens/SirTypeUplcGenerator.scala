package scalus.sir.lowering.typegens

import scalus.sir.*
//import scalus.sir.SIR.Pattern
//import scalus.sir.SIRVarStorage.{DEFAULT, Data, ScottEncoding}
//import scalus.sir.lowering.Lowering.{genError, lowerSIR, tpf}
import scalus.sir.lowering.*

trait SIRTypeUplcGenerator {

    /** Get default representation. Assumed that input parameters of functions and vars and are in
      * this representation.
      */
    def defaultRepresentation: LoweredValueRepresentation

    /** Get default representation for data representation of this type. This representation is used
      * when converting to data.
      */
    def defaultDataRepresentation: LoweredValueRepresentation

    def toRepresentation(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using
        lctx: LoweringContext
    ): LoweredValue

    /** Generate constructor for this type. Always called on DataDecl.tp
      */
    def genConstr(constr: SIR.Constr)(using
        LoweringContext
    ): LoweredValue

    def genSelect(sel: SIR.Select)(using LoweringContext): LoweredValue

    def genMatch(matchData: SIR.Match, loweredScrutinee: LoweredValue)(using
        LoweringContext
    ): LoweredValue

}

object SIRTypeUplcGenerator {

    def apply(tp: SIRType): SIRTypeUplcGenerator = {
        tp match
            case SIRType.Boolean =>
                SIRTypeUplcBooleanGenerator
            case SIRType.Integer =>
                SIRTypeUplcIntegerGenerator
            case _ =>
                ???
    }

}
