package scalus

import dotty.tools.dotc.*
import dotty.tools.dotc.core.*
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
                    makeSIRNonFunClassType(sym, Nil)
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
                            if (defn.isFunctionType(tpc)) then
                                makeSIRFunType(tp, env)
                            else
                                val sym = tpc.typeSymbol
                                makeSIRNonFunClassType(tpc.typeSymbol, tp.args.map(sirTypeInEnv(_, env)))
                        case tpc: TermRef =>
                            SIRType.TypeError(s"TermRef  ${tpc.show} not supported")
                        case other =>
                            SIRType.TypeError(s"AppliedType ${tp.show} not supported")
            case tp: AnnotatedType =>
                sirTypeInEnv(tp.underlying, env)
            case tp: AndType =>
                findClassInAndType(tp) match
                    case Some(tpf) => makeSIRClassTypeNoTypeArgs(tpf)
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
                makeSIRFunType(tpm, env)
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

    def makeSIRClassTypeNoTypeArgs(tp: Type)(using Context): SIRType = {
        if (defn.isFunctionType(tp)) then
            makeFunTypeLambda(tp)
        else
            makeSIRNonFunClassType(tp.typeSymbol, Nil)
    }

    def makeSIRNonFunClassType(sym: Symbol, types: List[SIRType])(using Context): SIRType = {
        println(s"makeSIRNonFumClassType ${sym.showFullName} ${types.map(_.show)}")
        (tryMakePrimitivePrimitive(sym, types) orElse
          tryMakeBuildinType(sym, types) orElse
          tryMakeCaseClassType(sym, types)
        ).getOrElse{
            val name = sym.showFullName
            val typeArgs = types.map(_.show)
            println(s"makeSIRClassType ${name} ${typeArgs}")
            SIRType.TypeError(s"Class $name with type args $typeArgs not supported")
        }
    }

    def makeSIRFunType(tp: Type, env: Map[Symbol, SIRType])(using Context): SIRType = {
        tp match
            case mt:MethodType =>
                makeSIRMethodType(mt)
            case AppliedType(tycon, args) =>
                if (defn.isFunctionType(tycon)) then
                    makeFunctionClassType(tycon.typeSymbol, args.map(sirTypeInEnv(_, env)))
                else
                    SIRType.TypeError(s"AppliedType ${tycon.show} not supported")
            case _ =>
                ???
    }

    def findClassInAndType(andType: AndType)(using Context): Option[Type] = {
        ???
    }

    def tryMakePrimitivePrimitive(symbol: Symbol, tpArgs: List[SIRType])(using Context): Option[SIRType] = {
        if (!tpArgs.isEmpty) then
            None
        else if (symbol == defn.BooleanType.typeSymbol || symbol == defn.BoxedBooleanClass) then
            Some(SIRType.BooleanPrimitive)
        else if (symbol ==  Symbols.requiredClass("scalus.builtin.ByteString") ) then
            Some(SIRType.ByteStringPrimitive)
        else if (symbol == Symbols.requiredClass("scala.math.BigInt") ) then
            Some(SIRType.IntegerPrimitive)
        else if (symbol == defn.StringType.typeSymbol) then
            Some(SIRType.StringPrimitive)
        else if (symbol == defn.UnitClass) then
            Some(SIRType.VoidPrimitive)
        else
            None
    }

    def tryMakeBuildinType(symbol: Symbol, tpArgs: List[SIRType])(using Context): Option[SIRType] = {
        if (symbol == Symbols.requiredClass("scalus.builtin.Data")) then
            Some(SIRType.Data)
        else if (symbol == Symbols.requiredClass("scalus.builtin.List")) then
            tpArgs match
                case List(elemType) => Some(SIRType.List(elemType))
                case _ =>
                    Some(SIRType.TypeError(s"List type should have one type argument, found ${tpArgs.length}"))
        else
            None
    }

    def makeFunctionClassType(symbol: Symbols.Symbol, list: List[SIRType])(using Context): SIRType = {
        val args = list.init
        val res = list.last
        makeUnaryFun(args, res)
    }


    /**
     * case classes and symbols.
     * @param symbol
     * @param tpArgs
     * @param x$3
     * @return
     */
    def tryMakeCaseClassType(symbol: Symbol, tpArgs: List[SIRType])(using Context): Option[SIRType] = {
        ???
    }

    def makeUnaryFun(args: List[SIRType], res: SIRType)(using Context): SIRType = {
        args match
            case Nil => res
            case head::Nil => SIRType.Fun(head, res)
            case head::tail => SIRType.Fun(head, makeUnaryFun(tail, res))
    }

    def makeSIRMethodType(mt: MethodType)(using Context): SIRType = {
        val params = mt.paramNames.zip(mt.paramInfos).map{
            (name, tp) => sirType(tp)
        }
        val res = sirType(mt.resultType)
        makeUnaryFun(params,res)
    }

    def makeFunTypeLambda(fn: Type): SIRType = ???



}
