package scalus

import dotty.tools.dotc.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Symbols.*

import scalus.sir.*


object SIRTypesHelper {

    // same as liftM
    def sirType(tp: Type)(using Context): SIRType =
        sirTypeInEnv(tp, Map.empty)

    def sirTypeInEnv(tp: Type, env: Map[Symbol, SIRType])(using Context): SIRType =
        tp.dealias.widen match
            case tpc: TermRef =>
                SIRType.TypeError(s"TermRef  ${tpc.show} not supported")
            case tpc: TypeRef =>
                val sym = tpc.typeSymbol
                if (sym.isClass) then
                    makeSIRClassType(sym, Nil)
                else
                    SIRType.TypeError(s"TypeRef ${tpc.show} not supported")
            case tpc: ConstantType =>
                // hmm, widen should have taken care of this
                ???
            case tpc: SuperType =>
                ???
            case tpc: RefinedType =>
                sirTypeInEnv(tpc.parent, env)
            case tp: AppliedType =>
                if (tp.tycon.isRef(defn.MatchCaseClass)) then
                    SIRType.TypeError("MatchCaseClass not supported")
                else
                    tp.tycon match
                        case tpc: TypeRef =>
                            val sym = tpc.typeSymbol
                            if (sym.isClass) then
                                makeSIRClassType(sym, tp.args.map(sirTypeInEnv(_, env)))
                            else
                                SIRType.TypeError(s"AppliedType ${tpc.show} not supported")
                        case tpc: TermRef =>
                            SIRType.TypeError(s"TermRef  ${tpc.show} not supported")
            case tp: AnnotatedType =>
                sirTypeInEnv(tp.underlying, env)
            case tp: AndType =>
                findClassInAndType(tp) match
                    case Some(sym) => makeSIRClassType(sym, Nil)
                    case None => SIRType.TypeError(s"AndType ${tp.show} not supported")
            case tp: OrType =>
                SIRType.TypeError(s"OrType ${tp.show} not supported")
            case tp: MatchType =>
                SIRType.TypeError(s"MatchType ${tp.show} not supported")
            case tp: ExprType =>
                sirTypeInEnv(tp.underlying, env)
            case tp: ParamRef =>
                val binder = tp.binder
                val paramName = binder.paramNames(tp.paramNum).show //TODO: better way to get the name as string
                SIRType.TypeVar(paramName, Some(tp.typeSymbol.hashCode))
            case tpc: ThisType =>
                sirTypeInEnv(tpc.underlying, env)
            case tpc: RecThis =>
                sirTypeInEnv(tpc.underlying, env)
            case tpc: RecType =>
                SIRType.TypeError(s"RecType ${tpc.show} not supported")
            case tpm: MethodType =>
                makeSIRFunType(tpm)
            case tpp: PolyType =>
                val params = (tpp.paramNames zip tpp.paramRefs).map{ (name, ref) =>
                    SIRType.TypeVar(name.show, Some(ref.typeSymbol.hashCode()))
                }
                SIRType.TypeLambda(params, sirTypeInEnv(tpp.resultType, env))
            case tpl: HKTypeLambda =>
                val params = (tpl.paramNames zip tpl.paramRefs).map{
                    (name, ref) => SIRType.TypeVar(name.show, Some(ref.typeSymbol.hashCode()))
                }
                SIRType.TypeLambda(params, sirTypeInEnv(tpl.resType, env))
            case tpp: TypeBounds =>
                SIRType.FreeUnificator
            case NoPrefix =>
                SIRType.TypeError(s"NoPrefix type is not supported")
            //case tpf: FlexibleType =>
            //    sirTypeInEnv(tpf.underlying, env)
            case other =>
                SIRType.TypeError(s"Not supported type ${other.show}")



    def makeSIRClassType(sym: Symbol, types: List[SIRType])(using Context): SIRType = {
        ???
    }

    def makeSIRFunType(methodType: MethodType)(using Context): SIRType = {
        ???
    }

    def findClassInAndType(andType: AndType)(using Context): Option[Symbol] = {
        ???
    }

}
