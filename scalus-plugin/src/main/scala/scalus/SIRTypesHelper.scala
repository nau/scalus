package scalus

import dotty.tools.dotc.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.util.{SrcPos}
import scalus.sir.*


object SIRTypesHelper {



    case class SIRTypeEnv(pos: SrcPos,
                          vars: Map[Symbol, SIRType],
                          forwardRefs : Map[Symbol,SIRType.TypeProxy] = Map.empty)

    case class TypingException(tpe: Type, pos: SrcPos, msg: String, cause: Throwable = null) extends RuntimeException(msg, cause)


    def sirTypeInEnv(tp: Type, env: SIRTypeEnv)(using Context): SIRType = {
        val retval =
            try
                sirTypeInEnvWithErr(tp.widen, env)
            catch
                case e: TypingException =>
                    println(s"typing exception during sirTypeInEnv(${tp.show}), tp tree: ${tp}")
                    throw e
        /*
        env.forwardRefs.get(wtp.typeSymbol) match
            case Some(proxy) => proxy
            case None =>
                val proxy = new SIRType.TypeProxy(null)
                try {
                    val retval = sirTypeInEnvWithErr(tp, env.copy(forwardRefs = env.forwardRefs.updated(wtp.typeSymbol, proxy)))
                    proxy.ref = retval
                    println(s"sirTypeInEnv for ${tp.show} return, retval=${retval}")
                    retval
                } catch {
                    case e: TypingException =>
                        println(s"typing exception during sirTypeInEnv(${tp.show}), tp tree: ${tp}")
                        throw e
                }

         */
        retval
    }

    def sirTypeInEnvWithErr(tp: Type, env: SIRTypeEnv)(using Context): SIRType =
        //println(s"sirTypeInEnvWithErr ${tp.show},  env=${env}")
        val retval = tp match
            case tpc: TermRef =>
                if (tpc.typeSymbol.isTypeParam) then
                    println("type parameter detected: " + tpc.show)
                    unsupportedType(tp, s"TermRef ${tpc.show}", env)
                else if !(tpc.widen =:= tpc) then
                    sirTypeInEnvWithErr(tp.widen, env)
                else
                    unsupportedType(tp, s"TermRef ${tpc.show}", env)
            case tpc: TypeRef =>
                val sym = tpc.typeSymbol
                if (tpc =:= defn.NothingType) then
                    SIRType.TypeNothing
                else if (tpc.isTypeAlias) then
                    sirTypeInEnvWithErr(tpc.dealias, env)
                else if (sym.isClass) then
                    makeSIRNonFunClassType(tpc, Nil, env)
                else if (sym.isTypeParam) then
                    env.vars.get(sym) match
                        case Some(t) => t
                        case None =>
                            val name = sym.showFullName
                            println(s"before unsupported type, sym=${sym}, sym.hashCode=${sym.hashCode()} name=${name}, env=${env}")
                            unsupportedType(tp, s"TypeRef ${tpc.show}", env)
                else if (sym.isAliasType) then
                    // looks like bug in a compiler
                    //println(s"stragen alias: ${tpc.show}, tpc.isTypeAlias=${tpc.isTypeAlias}, tp.isTypeAlias=${tp.isTypeAlias} sym.isAlias=${sym.isAliasType}, tpc.isSingleton=${tpc.isSingleton}")
                    //println(s"strange aliase: tree=${tpc}, symFullName=${sym.fullName}, dealias=${tpc.dealias.show}, deaslias.fullName=${tpc.dealias.typeSymbol.fullName}")
                    //throw new Exception("alias type")
                    sirTypeInEnvWithErr(tpc.dealias, env)
                else if (tpc.isValueType) then
                    makeSIRNonFunValueType(tpc, Nil, env)
                else
                    unsupportedType(tpc, "TypeRef", env)
            case tpc: ConstantType =>
                // hmm, widen should have taken care of this
                println(s"ConstantType ${tpc.show}")
                sirTypeInEnvWithErr(tpc.widen, env)
            case tpc: SuperType =>
                ???
            case tpc: RefinedType =>
                sirTypeInEnvWithErr(tpc.parent, env)
            case tp: AppliedType =>
                if (tp.tycon.isRef(defn.MatchCaseClass)) then
                    unsupportedType(tp,"MatchCaseClass",env)
                else
                    if (defn.isFunctionType(tp)) then
                        makeSIRFunType(tp, env)
                    else
                        tp.tycon match
                            case tpc: TypeRef =>
                                if (tpc.isTypeAlias || tpc.symbol.isAliasType) then
                                    sirTypeInEnvWithErr(tpc.dealias.appliedTo(tp.args), env)
                                else
                                    makeSIRNonFunClassType(tpc, tp.args.map(sirTypeInEnv(_, env)), env)
                            case tpc: TypeLambda =>
                                unsupportedType(tp, s"TypeLambda ${tpc.show}", env)
                            case tpc: TermRef =>
                                unsupportedType(tp, s"TermRef ${tpc.show}", env)
                            case other =>
                                unsupportedType(tp, s"AppliedType ${tp.show}", env)
            case tp: AnnotatedType =>
                sirTypeInEnvWithErr(tp.underlying, env)
            case tp: AndType =>
                findClassInAndType(tp) match
                    case Some(tpf) => makeSIRClassTypeNoTypeArgs(tpf, env)
                    case None =>
                        unsupportedType(tp, s"AndType", env)
            case orTp: OrType =>
                sirTypeInEnvWithErr(orTp.join, env)
            case tp: MatchType =>
                unsupportedType(tp, s"MatchType", env)
            case tp: ExprType =>
                sirTypeInEnvWithErr(tp.underlying, env)
            case tp: ParamRef =>
                val binder = tp.binder
                val paramName = binder.paramNames(tp.paramNum).show //TODO: better way to get the name as string
                SIRType.TypeVar(paramName, Some(tp.typeSymbol.hashCode))
            case tpc: ThisType =>
                sirTypeInEnv(tpc.underlying, env)
            case tpc: RecThis =>
                sirTypeInEnvWithErr(tpc.underlying, env)
            case tpc: RecType =>
                unsupportedType(tp, s"RecType", env)
            case tpm: MethodType =>
                makeSIRFunType(tpm, env)
            case tpp: PolyType =>
                val params = (tpp.paramNames zip tpp.paramRefs).map{ (name, ref) =>
                    SIRType.TypeVar(name.show, Some(ref.typeSymbol.hashCode()))
                }
                SIRType.TypeLambda(params, sirTypeInEnvWithErr(tpp.resultType, env))
            case tpl: HKTypeLambda =>
                val params = (tpl.paramNames zip tpl.paramRefs).map{
                    (name, ref) => SIRType.TypeVar(name.show, Some(ref.typeSymbol.hashCode()))
                }
                SIRType.TypeLambda(params, sirTypeInEnvWithErr(tpl.resType, env))
            case tpp: TypeBounds =>
                SIRType.FreeUnificator
            case NoPrefix =>
                unsupportedType(tp, "NoPrefix", env)
            //case tpf: FlexibleType =>
            //    sirTypeInEnv(tpf.underlying, env)
            case classInfo: ClassInfo =>
                makeSIRNonFunClassType(classInfo.appliedRef, Nil, env)
            case typeVar: TypeVar =>
                env.vars.get(typeVar.typeSymbol) match
                    case Some(t) => t
                    case None =>
                        //TODO: think, matbe better create a new TypeVar with FreeUnificator as value ?
                        val name = typeVar.typeSymbol.showFullName
                        unsupportedType(tp, s"TypeVar ${typeVar.show}, name=${name}", env)
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
        val retval = (tryMakePrimitivePrimitive(sym, types) orElse
          tryMakeBuildinType(sym, types, env) orElse
          tryMakeCaseClassOrCaseParent(sym, types, env) orElse
          tryMakeNonCaseModule(tp, sym, types, env)
        ).getOrElse{
            val name = sym.showFullName
            val typeArgs = types.map(_.show)
            println(s"makeSIRClassType ${name} ${typeArgs}")
            unsupportedType(tp,s"tree=${tp}, isClass=${sym.isClass} isAliasType=${sym.isAliasType}, info:${sym.info}", env)
        }
        println("nakeSIRNonFunClassType return " + retval)
        retval
    }

    def makeSIRNonFunValueType(tpc: TypeRef, params: List[Type], env: SIRTypeEnv)(using Context): SIRType = {
        val sym = tpc.typeSymbol
        if (sym == Symbols.requiredClass("scala.math.BigInt")) then
           SIRType.IntegerPrimitive
        else
          unsupportedType(tpc, "ValueType", env)
    }

    def makeSIRFunType(tp: Type, env: SIRTypeEnv)(using Context): SIRType = {
        tp match
            case mt:MethodType =>
                makeSIRMethodType(mt, env)
            case AppliedType(tycon, args) =>
                if (defn.isFunctionType(tp)) then
                    println(s"makeSIRFunType AppliedType for ${tycon.show} ${args.map(_.show)}")
                    val retval = makeFunctionClassType(tycon.typeSymbol, args.map(sirTypeInEnvWithErr(_, env)), env)
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
        else if (symbol == defn.IntType.typeSymbol || symbol == defn.BoxedIntClass) then
            Some(SIRType.IntegerPrimitive)
        else if (symbol == defn.LongType.typeSymbol || symbol == defn.BoxedLongClass) then
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
                println(s"tryMakeCaseClassOrCaseParent ${typeSymbol.showFullName} ${tpArgs.map(_.show)}")
                val proxy = new SIRType.TypeProxy(null)
                val retval = tryMakeCaseClassOrCaseParentTypeNoRec(typeSymbol, tpArgs, env.copy(forwardRefs = env.forwardRefs.updated(typeSymbol, proxy)), proxy)
                retval match
                    case Some(t) =>
                        proxy.ref = t
                        Some(t)
                    case None =>
                        None
    }


    /**
     * case classes and symbols.
     * @param typeSymbol
     * @param tpArgs
     * @param x$3
     * @return
     */
    def tryMakeCaseClassOrCaseParentTypeNoRec(typeSymbol:Symbol, tpArgs: List[SIRType], env: SIRTypeEnv, thisProxy: SIRType.TypeProxy)(using Context): Option[SIRType] = {
        println(s"tryMakeCaseClassOrCaseParentTypeNoRec ${typeSymbol.showFullName} ${tpArgs.map(_.show)}, isCase=${typeSymbol.flags.is(Flags.CaseClass)}, isEnum=${typeSymbol.flags.is(Flags.Enum)}, flags=${typeSymbol.flagsString}")
        println(s"typeSymbol.isType=${typeSymbol.isType}, typeSymbol.isClass=${typeSymbol.isClass}, typeSymbol.isTerm=${typeSymbol.isTerm}")
        if (typeSymbol.flags.is(Flags.Case) || typeSymbol.flags.is(Flags.Enum)) {
                // case class, can do constrdecl
                val name = typeSymbol.fullName.show
                val tparams = typeSymbol.info.typeParamSymbols.map(s => SIRType.TypeVar(s.name.show, Some(s.hashCode)))
                val nVars = typeSymbol.info.typeParamSymbols.zip(tparams).foldLeft(env.vars) {
                    case (acc, (sym, tvar)) => acc.updated(sym, tvar)
                }
                val nEnv = env.copy(vars = nVars)
                val params = typeSymbol.info.fields.map(f => TypeBinding(f.name.show, sirTypeInEnv(f.info, nEnv))).toList
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
                Some(nType)
        } else {
                // TODO: keep in env mapSymbol => SumCaseClass to prevent duplication
                if (typeSymbol.children.nonEmpty) {
                    val childrenSymbols = typeSymbol.children
                    val childrenTypes = childrenSymbols.map(s =>
                        println(s"run childrenTypr for $s, s.info=${s.info.show}, s.isType=${s.isType}, s.isTerm = ${s.isTerm}")
                        if (s.isType) then
                            sirTypeInEnv(s.info, env)
                        else
                            val parentTpArgs = s.info.baseType(typeSymbol) match
                                case AppliedType(tycon, args) => args.map(sirTypeInEnv(_, env))
                                case _ => Nil
                            val constrDecl = ConstrDecl(s.name.show, SIRVarStorage.DEFAULT, Nil, Nil, parentTpArgs)
                            SIRType.CaseClass(constrDecl, Nil)
                    )
                    childrenTypes.find{ chtp =>
                        ! chtp.isInstanceOf[SIRType.CaseClass] &&
                        ! chtp.isInstanceOf[SIRType.SumCaseClass] &&
                        ! chtp.isInstanceOf[SIRType.TypeLambda] &&
                        ! chtp.isInstanceOf[SIRType.TypeProxy]
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
                                val unfinishedConstr = childrenTypes.filter{
                                        case SIRType.TypeProxy(ref) => ref == null
                                        case _ => false
                                }
                                if (unfinishedConstr.nonEmpty) {
                                    val unfinishedSymbols = env.forwardRefs.flatMap{ case (k,v) =>
                                        if (unfinishedConstr.contains(v)) {
                                            Some(k)
                                        } else {
                                            None
                                        }
                                    }
                                    println(s"thisType = ${typeSymbol} (${typeSymbol.hashCode()}) , unfinishedSymbols=${unfinishedSymbols.map(s => (s.hashCode(), s.fullName))}")
                                    println(s"childrenSymbols= ${childrenSymbols.map(s => (s.hashCode(), s.fullName))}")
                                    println(s"childrenTypes= ${childrenTypes.map(_.show)}")
                                    ???
                                }
                                val nType =
                                    try
                                        val constrDecls = childrenTypes.map {
                                            case chtp: SIRType.CaseClass => chtp.constrDecl
                                            case tpr: SIRType.TypeProxy if (tpr.ref.isInstanceOf[SIRType.CaseClass]) => tpr.ref.asInstanceOf[SIRType.CaseClass].constrDecl
                                            case nullProxy: SIRType.TypeProxy if (nullProxy.ref == null) =>
                                                // tpr
                                                ???
                                            case other =>
                                                throw TypingException(typeSymbol.info, env.pos, s"Case parent type ${typeSymbol.showFullName} has children that are not case classes or case parent types: ${other.show}")
                                                //typeError(typeSymbol.info, s"Case parent type ${typeSymbol.showFullName} has children that are not case classes or case parent types: ${other.show}", env, throwError = true)
                                        }
                                        SIRType.SumCaseClass(DataDecl(name, constrDecls, tparams), tpArgs)
                                    catch
                                        case e: TypingException =>
                                            thisProxy.ref = SIRType.TypeError(e.msg, e)
                                            typeError(typeSymbol.info, e.msg, env, throwError = true, e)
                                            throw e;
                                thisProxy.ref = nType
                                Some(nType)
                            }
                } else {
                    None
                }
        }
    }

    def tryMakeNonCaseModule(tp: Type, typeSymbol: Symbol, tpArgs: List[SIRType], env: SIRTypeEnv)(using Context): Option[SIRType] = {
        println(s"tryMakeNonCaseModule ${typeSymbol.showFullName} ${tpArgs.map(_.show)}, flags:${typeSymbol.flagsString}, isModule=${typeSymbol.flags.is(Flags.Module)}")
        if (typeSymbol.flags.is(Flags.Module)||typeSymbol.flags.is(Flags.Package)) {
            val name = typeSymbol.fullName.show
            if (!tpArgs.isEmpty) {
                val msg = s"Module type ${typeSymbol.showFullName} should not have type arguments"
                Some(typeError(tp, msg, env, throwError = true))
            } else {
                Some(SIRType.TypeNonCaseModule(name))
            }
        } else {
            None
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
                (_, tp) => sirTypeInEnvWithErr(tp, env)
            }
            val res = sirTypeInEnvWithErr(mt.resultType, env)
            makeUnaryFun(params, res)
        }catch{
            case e: TypingException =>
                println(s"makeSIRMethodType exception ${e.msg}")
                println(s"mt=paramInfps=${mt.paramInfos.map(_.show)}")
                throw e;
        }
    }

    def makeFunTypeLambda(fn: Type): SIRType = ???

    def typeError(tpe: Type, msg: String, env: SIRTypeEnv, throwError: Boolean = true, cause: Throwable = null)(using Context): SIRType = {
        if (throwError) then
            throw TypingException(tpe, env.pos, msg)
        else
            SIRType.TypeError(msg, null)
    }


    def unsupportedType(tpe: Type, msg: String, env: SIRTypeEnv, throwError: Boolean = true)(using Context): SIRType = {
        typeError(tpe, s"unsupported type: ${tpe.show} $msg", env, throwError)
    }

}
