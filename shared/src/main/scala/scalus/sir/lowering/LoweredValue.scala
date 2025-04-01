package scalus.sir.lowering

import scalus.sir.*
import scalus.uplc.*

case class LoweredValue(
    sir: SIR,
    term: Term,
    representation: SIRVarStorage,
    dataShouldBeChecked: Boolean = false,
    delayed: Int = 0,
    forced: Boolean = false
) {

    def asData(using LoweringContext): LoweredValue = {
        SIRTypeUplcGenerator(sir.tp).toData(this)
    }

    def asTerm(using LoweringContext): LoweredValue = {
        SIRTypeUplcGenerator(sir.tp).toTerm(this)
    }

    def toRepresnetation(representation: SIRVarStorage)(using LoweringContext): LoweredValue = {
        representation match
            case SIRVarStorage.Data      => asData
            case SIRVarStorage.LocalUPLC => asTerm
    }

}

object LoweredValue {

    def intConstant(n: Int): LoweredValue = {
        LoweredValue(
          SIR.Const(Constant.Integer(n), SIRType.Integer, AnnotationsDecl.empty),
          Term.Const(Constant.Integer(n)),
          SIRVarStorage.LocalUPLC
        )
    }

    def boolConstant(b: Boolean): LoweredValue = {
        LoweredValue(
          SIR.Const(Constant.Bool(b), SIRType.Boolean, AnnotationsDecl.empty),
          Term.Const(Constant.Bool(b)),
          SIRVarStorage.LocalUPLC
        )
    }

}
