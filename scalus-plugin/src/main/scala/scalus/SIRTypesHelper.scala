package scalus

import dotty.tools.dotc.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.util.{SourcePosition, SrcPos}
import scalus.sir.*


object SIRTypesHelper {



    case class SIRTypeEnv(pos: SrcPos,
                          vars: Map[Symbol, SIRType],
                          forwardRefs : Map[Symbol,SIRType.TypeProxy] = Map.empty)

    case class TypingException(tpe: Type, pos: SrcPos, msg: String) extends RuntimeException(msg)


    def sirTypeInEnv(tp: Type, env: SIRTypeEnv)(using Context): SIRType = {
        env.forwardRefs.get(tp.typeSymbol) match
            case Some(proxy) => proxy
            case None =>
                val proxy = new SIRType.TypeProxy(null)
                try {
                    val retval = sirTypeInEnvWithErr(tp, env.copy(forwardRefs = env.forwardRefs.updated(tp.typeSymbol, proxy)))
                    proxy.ref = retval
                    println(s"sirTypeInEnv for ${tp.show} return, retval=${retval}")
                    retval
                } catch {
                    case e: TypingException =>
                        println(s"typing exception during sirTypeInEnv(${tp.show}), tp tree: ${tp}")
                        throw e
                }
    }

    def sirTypeInEnvWithErr(tp: Type, env: SIRTypeEnv)(using Context): SIRType =
        val retval = tp.dealias.widen match
            case tpc: TermRef =>
                if (tpc.typeSymbol.isTypeParam) then
                    println("type parameter detected: " + tpc.show)
                    unsupportedType(tp, s"TermRef ${tpc.show}", env)
                else
                    unsupportedType(tp, s"TermRef ${tpc.show}", env)
            case tpc: TypeRef =>
                val sym = tpc.typeSymbol
                if (tpc =:= defn.NothingType) then
                    SIRType.TypeNothing
                else if (sym.isClass) then
                    makeSIRNonFunClassType(tpc, Nil, env)
                else if (sym.isTypeParam) then
                    env.vars.get(sym) match
                        case Some(t) => t
                        case None =>
                            val name = sym.showFullName
                            println(s"makeSIRClassType ${name}")
                            unsupportedType(tp, s"TypeRef ${tpc.show}", env)
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
                    if (defn.isFunctionType(tp)) then
                        makeSIRFunType(tp, env)
                    else
                        tp.tycon match
                            case tpc: TypeRef =>
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
                unsupportedType(tp, s"${tp.show}, tree=${tp}", env)
        retval

    def makeSIRClassTypeNoTypeArgs(tp: Type, env: SIRTypeEnv)(using Context): SIRType = {
        if (defn.isFunctionType(tp)) then
            makeFunTypeLambda(tp)
        else
            makeSIRNonFunClassType(tp, Nil, env)
    }

    def makeSIRNonFunClassType(tp: Type, types: List[SIRType], env: SIRTypeEnv)(using Context): SIRType = {
        val sym = tp.typeSymbol
        println(s"makeSIRNonFumClassType ${sym.showFullName} ${types.map(_.show)}, isFunctionType=${defn.isFunctionType(tp)}")
        (tryMakePrimitivePrimitive(sym, types) orElse
          tryMakeBuildinType(sym, types, env) orElse
          tryMakeCaseClassOrCaseParent(sym, types, env)
        ).getOrElse{
            val name = sym.showFullName
            val typeArgs = types.map(_.show)
            println(s"makeSIRClassType ${name} ${typeArgs}")
            unsupportedType(tp,s"tree=${tp}, isClass=${sym.isClass} isAliasType=${sym.isAliasType}", env)
        }
    }

    def makeSIRFunType(tp: Type, env: SIRTypeEnv)(using Context): SIRType = {
        tp match
            case mt:MethodType =>
                makeSIRMethodType(mt, env)
            case AppliedType(tycon, args) =>
                if (defn.isFunctionType(tp)) then
                    println(s"makeSIRFunType AppliedType for ${tycon.show} ${args.map(_.show)}")
                    val retval = makeFunctionClassType(tycon.typeSymbol, args.map(sirTypeInEnv(_, env)), env)
                    retval
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

    def tryMakeCaseClassOrCaseParent(typeSymbol:Symbol, tpArgs: List[SIRType], env: SIRTypeEnv)(using Context): Option[SIRType] = {
        env.forwardRefs.get(typeSymbol) match
            case Some(proxy) =>
                Some(proxy)
            case None =>
                tryMakeCaseClassOrCaseParentTypeNoRec(typeSymbol, tpArgs, env)
    }


    /**
     * case classes and symbols.
     * @param typeSymbol
     * @param tpArgs
     * @param x$3
     * @return
     */
    def tryMakeCaseClassOrCaseParentTypeNoRec(typeSymbol:Symbol, tpArgs: List[SIRType], env: SIRTypeEnv)(using Context): Option[SIRType] = {
        println(s"tryMakeCaseClassOrCaseParentType ${typeSymbol.showFullName} ${tpArgs.map(_.show)}")
        if (typeSymbol.flags.is(Flags.Case)) {
                // case class, can do constrdecl
                val thisProxy = SIRType.TypeProxy(null)
                val nEnv1 = env.copy(forwardRefs = env.forwardRefs.updated(typeSymbol, thisProxy))
                val name = typeSymbol.fullName.show
                val params = typeSymbol.info.fields.map(f => TypeBinding(f.name.show, sirTypeInEnv(f.info, env))).toList
                val tparams = typeSymbol.info.typeParamSymbols.map(s => SIRType.TypeVar(s.name.show, Some(s.hashCode)))
                val nVars = typeSymbol.info.typeParamSymbols.zip(tparams).foldLeft(env.vars) {
                    case (acc, (sym, tvar)) => acc.updated(sym, tvar)
                }
                val nEnv = env.copy(vars = nVars)
                val optBaseSymbol = typeSymbol.info.baseClasses.find(bc => bc.children.nonEmpty)
                val constrDecl = optBaseSymbol match
                    case Some(baseSymbol) =>
                        val baseClassType = typeSymbol.info.baseType(baseSymbol)
                        val parentTParams = baseClassType match
                            case AppliedType(tycon, args) => args.map(sirTypeInEnv(_, nEnv))
                            case _ => Nil
                        ConstrDecl(name, SIRVarStorage.Data, params, tparams, parentTParams)
                    case None =>
                        ConstrDecl(name, SIRVarStorage.Data, params, tparams, Nil)
                val nType = SIRType.CaseClass(constrDecl, tpArgs)
                thisProxy.ref = nType
                Some(nType)
        } else {
                if (typeSymbol.children.nonEmpty) {
                    val thisProxy = SIRType.TypeProxy(null)
                    val childrenSymbols = typeSymbol.children
                    val childrenTypes = childrenSymbols.map(s => sirTypeInEnv(s.info, env))
                    childrenTypes.find{ chtp =>
                        ! chtp.isInstanceOf[SIRType.CaseClass] &&
                        ! chtp.isInstanceOf[SIRType.SumCaseClass] &&
                        ! chtp.isInstanceOf[SIRType.TypeLambda]
                    } match
                        case Some(errType) =>
                            val msg = s"Case parent type ${typeSymbol.showFullName} has children that are not case classes or case parent types: ${errType.show}"
                            thisProxy.ref = SIRType.TypeError(msg, null)
                            Some(typeError(typeSymbol.info, msg, env, throwError = true))
                        case None =>
                            val name = typeSymbol.fullName.show
                            val tparams = typeSymbol.info.typeParamSymbols.map(s => SIRType.TypeVar(s.name.show, Some(s.hashCode)))
                            if (tparams.length != tpArgs.length) {
                                val msg = s"Case parent type ${typeSymbol.showFullName} has ${tparams.length} type parameters, but ${tpArgs.length} were provided"
                                thisProxy.ref = SIRType.TypeError(msg, null)
                                Some(typeError(typeSymbol.info, msg, env, throwError = true))
                            } else {
                                val nType =
                                    try
                                        val constrDecls = childrenTypes.map {
                                            case chtp: SIRType.CaseClass => chtp.constrDecl
                                            case tpr: SIRType.TypeProxy if (tpr.ref.isInstanceOf[SIRType.CaseClass]) => tpr.ref.asInstanceOf[SIRType.CaseClass].constrDecl
                                            case other =>
                                                throw TypingException(typeSymbol.info, env.pos, s"Case parent type ${typeSymbol.showFullName} has children that are not case classes or case parent types: ${other.show}")
                                                //typeError(typeSymbol.info, s"Case parent type ${typeSymbol.showFullName} has children that are not case classes or case parent types: ${other.show}", env, throwError = true)
                                        }
                                        SIRType.SumCaseClass(DataDecl(name, constrDecls, tparams), tpArgs)
                                    catch
                                        case e: TypingException =>
                                            thisProxy.ref = SIRType.TypeError(e.msg, null)
                                            typeError(typeSymbol.info, e.msg, env, throwError = true)
                                thisProxy.ref = nType
                                Some(nType)
                            }
                } else {
                    None
                }
        }
    }

    def makeUnaryFun(args: List[SIRType], res: SIRType)(using Context): SIRType = {
        args match
            case Nil => res
            case head::Nil => SIRType.Fun(head, res)
            case head::tail => SIRType.Fun(head, makeUnaryFun(tail, res))
    }

    def makeSIRMethodType(mt: MethodType, env: SIRTypeEnv)(using Context): SIRType = {
        println(s"makeSIRMethodType ${mt.show}")
        try {
            val params = mt.paramNames.zip(mt.paramInfos).map {
                (name, tp) => sirTypeInEnv(tp, env)
            }
            val res = sirTypeInEnv(mt.resultType, env)
            makeUnaryFun(params, res)
        }catch{
            case e: TypingException =>
                println(s"makeSIRMethodType exception ${e.msg}")
                println(s"mt=paramInfps=${mt.paramInfos.map(_.show)}")
                throw e;
        }
    }

    def makeFunTypeLambda(fn: Type): SIRType = ???

    def typeError(tpe: Type, msg: String, env: SIRTypeEnv, throwError: Boolean = false)(using Context): SIRType = {
        if (throwError) then
            throw TypingException(tpe, env.pos, msg)
        else
            SIRType.TypeError(msg, null)
    }


    def unsupportedType(tpe: Type, msg: String, env: SIRTypeEnv, throwError: Boolean = true)(using Context): SIRType = {
        typeError(tpe, s"unsupported type: ${tpe.show} $msg", env, throwError)
    }

}
