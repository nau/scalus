package scalus.sir.lowering.typegens

import scalus.sir.*

import java.util.IdentityHashMap
//import scalus.sir.SIR.Pattern
//import scalus.sir.SIRVarStorage.{DEFAULT, Data, ScottEncoding}
//import scalus.sir.lowering.Lowering.{genError, lowerSIR, tpf}
import scalus.sir.lowering.*

trait SirTypeUplcGenerator {

    /** Get default representation. Assumed that input parameters of functions and vars and are in
      * this representation.
      */
    def defaultRepresentation(tp: SIRType)(using LoweringContext): LoweredValueRepresentation

    /** Get default representation for data representation of this type. This representation is used
      * when converting to data.
      */
    def defaultDataRepresentation(tp: SIRType)(using LoweringContext): LoweredValueRepresentation

    /** Get default representation for type variable. This representation is used when converting to
      * parameters of scala functions with type parameters. (Usually the same as
      * defaultDataRepresentation, except Lambdas).
      */
    def defaultTypeVarReperesentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation

    def isDataSupported(tp: SIRType)(using
        lctx: LoweringContext
    ): Boolean

    def toRepresentation(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using
        lctx: LoweringContext
    ): LoweredValue

    def upcastOne(input: LoweredValue, targetType: SIRType, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue

    /** Generate constructor for this type. Always called on DataDecl.tp
      *
      * @param constr - constructor, which we should generate
      * @param targetType - type of the generated value, which should be a subtype of constr.tp
      */
    def genConstr(constr: SIR.Constr)(using
        LoweringContext
    ): LoweredValue

    def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        LoweringContext
    ): LoweredValue

    def genMatch(matchData: SIR.Match, loweredScrutinee: LoweredValue)(using
        LoweringContext
    ): LoweredValue

}

object SirTypeUplcGenerator {

    def apply(tp: SIRType): SirTypeUplcGenerator = {
        tp match
            case SIRType.Boolean =>
                SIRTypeUplcBooleanGenerator
            case SIRType.Integer =>
                SIRTypeUplcIntegerGenerator
            case SIRType.ByteString =>
                SIRTypeUplcByteStringGenerator
            case SIRType.String =>
                SIRTypeUplcStringGenerator
            case SIRType.Data =>
                SIRTypeUplcDataGenerator
            case SIRType.Unit =>
                UnitSirTypeGenerator
            case SIRType.SumCaseClass(decl, typeArgs) =>
                val trace = new IdentityHashMap[SIRType, SIRType]()
                if decl.name == "scalus.prelude.List" then
                    if !containsFun(tp, trace) then SumDataListSirTypeGenerator
                    else SumCaseUplcOnlySirTypeGenerator
                else if !containsFun(tp, trace) then SumCaseSirTypeGenerator
                else SumCaseUplcOnlySirTypeGenerator
            case SIRType.CaseClass(constrDecl, typeArgs, optParent) =>
                // TODO: retrieve annotation
                if constrDecl.name == "scalus.ledger.api.v1.PubKeyHash" then
                    ProductCaseOneElementSirTypeGenerator(SIRTypeUplcByteStringGenerator)
                else
                    val hasFun = containsFun(constrDecl, new IdentityHashMap[SIRType, SIRType]())
                    if constrDecl.name == "scalus.prelude.List$.Nil" || constrDecl.name == "scalus.prelude.List$.Cons"
                    then
                        if hasFun then SumCaseUplcOnlySirTypeGenerator
                        else SumDataListSirTypeGenerator
                    else if hasFun then ProductCaseUplcOnlySirTypeGenerator
                    else ProductCaseSirTypeGenerator
            case SIRType.TypeLambda(_, body) =>
                SirTypeUplcGenerator(body)
            case SIRType.TypeProxy(ref) =>
                SirTypeUplcGenerator(ref)
            case SIRType.Fun(input, output) =>
                FunSirTypeGenerator
            case SIRType.TypeVar(_, _, _) =>
                TypeVarSirTypeGenerator
            case SIRType.FreeUnificator =>
                TypeVarSirTypeGenerator
            case SIRType.TypeNothing =>
                TypeNothingSirTypeGenerator
    }

    private def containsFun(
        types: List[SIRType],
        trace: IdentityHashMap[SIRType, SIRType]
    ): Boolean =
        types.exists(tp => containsFun(tp, trace))

    private def containsFun(tp: SIRType, trace: IdentityHashMap[SIRType, SIRType]): Boolean = {
        if !(trace.get(tp) eq null) then false
        else
            tp match
                case SIRType.Fun(_, _) => true
                case SIRType.TypeLambda(_, body) =>
                    trace.put(tp, tp)
                    containsFun(body, trace)
                case SIRType.SumCaseClass(decl, typeArgs) =>
                    trace.put(tp, tp)
                    decl.constructors.exists(constr => containsFun(constr, trace)) || typeArgs
                        .exists(ta => containsFun(ta, trace))
                case SIRType.CaseClass(constrDecl, typeArgs, _) =>
                    trace.put(tp, tp)
                    typeArgs.exists(ta => containsFun(ta, trace)) || containsFun(constrDecl, trace)
                case SIRType.TypeProxy(ref) =>
                    containsFun(ref, trace)
                case _ => false
    }

    private def containsFun(
        constr: SIR.Constr,
        trace: IdentityHashMap[SIRType, SIRType]
    ): Boolean = {
        constr.args.exists(arg => containsFun(arg.tp, trace))
    }

    private def containsFun(
        constrDecl: ConstrDecl,
        trace: IdentityHashMap[SIRType, SIRType]
    ): Boolean = {
        constrDecl.params.exists(tpb => containsFun(tpb.tp, trace)) ||
        constrDecl.typeParams.exists(tp => containsFun(tp, trace))
    }

}
