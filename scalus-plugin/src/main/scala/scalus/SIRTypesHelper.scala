package scalus

import dotty.tools.dotc.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.util.{SourcePosition, SrcPos}
import scalus.sir.*


object SIRTypesHelper {



    case class SIRTypeEnv(pos: SrcPos, vars: Map[Symbol, SIRType])

    // same as liftM
    def sirType(tp: Type, pos: SrcPos)(using Context): SIRType =
        sirTypeInEnv(tp, SIRTypeEnv(pos, Map.empty))

    def sirTypeInEnv(tp: Type, env: SIRTypeEnv)(using Context): SIRType =
        tp.dealias.widen match
            case tpc: TermRef =>
                if (tpc.typeSymbol.isTypeParam) then
                    println("type parameter detected: " + tpc.show)
                    unsupportedType(tp, s"TermRef ${tpc.show}", env)
                else
                    unsupportedType(tp, s"TermRef ${tpc.show}", env)
            case tpc: TypeRef =>
                val sym = tpc.typeSymbol
                if (sym.isClass) then
                    makeSIRNonFunClassType(tpc, Nil, env)
                else if (sym.isTypeParam) then
                    ???
                else
                    unsupportedType(tpc, "TypeRef", env)
            case tpc: ConstantType =>
                // hmm, widen should have taken care of this
                ???
            case tpc: SuperType =>
                ???
            case tpc: RefinedType =>
                sirTypeInEnv(tpc.parent, env)
            case tp: AppliedType =>
                if (tp.tycon.isRef(defn.MatchCaseClass)) then
                    unsupportedType(tp,"MatchCaseClass",env)
                else
                    tp.tycon match
                        case tpc: TypeRef =>
                            if (defn.isFunctionType(tpc)) then
                                makeSIRFunType(tp, env)
                            else
                                val sym = tpc.typeSymbol
                                makeSIRNonFunClassType(tpc, tp.args.map(sirTypeInEnv(_, env)), env)
                        case tpc: TypeLambda =>
                            unsupportedType(tp, s"TypeLambda ${tpc.show}", env)
                        case tpc: TermRef =>
                            unsupportedType(tp, s"TermRef ${tpc.show}", env)
                        case other =>
                            unsupportedType(tp, s"AppliedType ${tp.show}", env)
            case tp: AnnotatedType =>
                sirTypeInEnv(tp.underlying, env)
            case tp: AndType =>
                findClassInAndType(tp) match
                    case Some(tpf) => makeSIRClassTypeNoTypeArgs(tpf, env)
                    case None =>
                        unsupportedType(tp, s"AndType", env)
            case orTp: OrType =>
                sirTypeInEnv(orTp.join, env)
            case tp: MatchType =>
                unsupportedType(tp, s"MatchType", env)
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
                unsupportedType(tp, s"RecType", env)
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
                unsupportedType(tp, "NoPrefix", env)
            //case tpf: FlexibleType =>
            //    sirTypeInEnv(tpf.underlying, env)
            case other =>
                unsupportedType(tp, s"$tp", env)

    def makeSIRClassTypeNoTypeArgs(tp: Type, env: SIRTypeEnv)(using Context): SIRType = {
        if (defn.isFunctionType(tp)) then
            makeFunTypeLambda(tp)
        else
            makeSIRNonFunClassType(tp, Nil, env)
    }

    def makeSIRNonFunClassType(tp: Type, types: List[SIRType], env: SIRTypeEnv)(using Context): SIRType = {
        val sym = tp.typeSymbol
        println(s"makeSIRNonFumClassType ${sym.showFullName} ${types.map(_.show)}")
        (tryMakePrimitivePrimitive(sym, types) orElse
          tryMakeBuildinType(sym, types, env) orElse
          tryMakeCaseClassType(sym, types, env)
        ).getOrElse{
            val name = sym.showFullName
            val typeArgs = types.map(_.show)
            println(s"makeSIRClassType ${name} ${typeArgs}")
            unsupportedType(tp,"", env)
        }
    }

    def makeSIRFunType(tp: Type, env: SIRTypeEnv)(using Context): SIRType = {
        tp match
            case mt:MethodType =>
                makeSIRMethodType(mt, env)
            case AppliedType(tycon, args) =>
                if (defn.isFunctionType(tycon)) then
                    makeFunctionClassType(tycon.typeSymbol, args.map(sirTypeInEnv(_, env)), env)
                else
                    unsupportedType(tp, "AppliedType as function", env)
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

    def tryMakeBuildinType(symbol: Symbol, tpArgs: List[SIRType], env: SIRTypeEnv)(using Context): Option[SIRType] = {
        if (symbol == Symbols.requiredClass("scalus.builtin.Data")) then
            Some(SIRType.Data)
        else if (symbol == Symbols.requiredClass("scalus.builtin.List")) then
            tpArgs match
                case List(elemType) => Some(SIRType.List(elemType))
                case _ =>
                    val err = SIRType.TypeError(s"List type should have one type argument, found ${tpArgs.length}", null)
                    Some(err)
        else if (symbol == Symbols.requiredClass("scalus.builtin.Pair")) then
            tpArgs match
                case List(a1,a2) => Some(SIRType.Pair(a1,a2))
                case _ =>
                    Some(SIRType.TypeError(s"Pair type should have two type arguments, found ${tpArgs.length}", null))
        else
            None
    }

    def makeFunctionClassType(symbol: Symbols.Symbol, list: List[SIRType], env: SIRTypeEnv)(using Context): SIRType = {
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
    def tryMakeCaseClassType(symbol: Symbol, tpArgs: List[SIRType], env: SIRTypeEnv)(using Context): Option[SIRType] = {
        println(s"tryMakeCaseClassType ${symbol.showFullName} ${tpArgs.map(_.show)}")
        ???
    }

    def makeUnaryFun(args: List[SIRType], res: SIRType)(using Context): SIRType = {
        args match
            case Nil => res
            case head::Nil => SIRType.Fun(head, res)
            case head::tail => SIRType.Fun(head, makeUnaryFun(tail, res))
    }

    def makeSIRMethodType(mt: MethodType, env: SIRTypeEnv)(using Context): SIRType = {
        val params = mt.paramNames.zip(mt.paramInfos).map{
            (name, tp) => sirTypeInEnv(tp, env)
        }
        val res = sirTypeInEnv(mt.resultType, env)
        makeUnaryFun(params,res)
    }

    def makeFunTypeLambda(fn: Type): SIRType = ???

    def typeError(tpe: Type, msg: String, env: SIRTypeEnv, throwError: Boolean = false)(using Context): SIRType = {
        if (throwError) then
            throw RuntimeException(UnsupportedType(tpe, env.pos, msg).message)
        else
            SIRType.TypeError(msg, null)
    }


    def unsupportedType(tpe: Type, msg: String, env: SIRTypeEnv, throwError: Boolean = true)(using Context): SIRType = {
        typeError(tpe, s"unsupported type: ${tpe.show} $msg", env, throwError)
    }

}
