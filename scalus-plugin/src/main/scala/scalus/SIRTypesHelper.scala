package scalus

import dotty.tools.dotc.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.util.SrcPos
import scalus.sir.*

object SIRTypesHelper {

    case class SIRTypeEnv(
        pos: SrcPos,
        vars: Map[Symbol, SIRType],
        forwardRefs: Map[Symbol, SIRType.TypeProxy] = Map.empty,
        trace: Boolean = false
    )

    case class TypingException(tpe: Type, pos: SrcPos, msg: String, cause: Throwable = null)
        extends RuntimeException(msg, cause)

    def sirTypeInEnv(tp: Type, env0: SIRTypeEnv)(using Context): SIRType = {
        val env =
            if tp =:= Symbols.requiredClass("scalus.ledger.api.v3.TxInfo").info then
                env0.copy(trace = true)
            else env0
        val retval =
            try sirTypeInEnvWithErr(tp.widen, env)
            catch
                case e: TypingException =>
                    println(s"typing exception during sirTypeInEnv(${tp.show}), tp tree: ${tp}")
                    throw e
        if true then
            if (!SIRType.checkAllProxiesFilled(retval)) then
                throw new TypingException(tp, env.pos, s"Unfilled proxies in ${retval.show}")
        retval
    }

    def sirTypeInEnvWithErr(tp: Type, env: SIRTypeEnv)(using Context): SIRType =
        if env.trace then println(s"sirTypeInEnvWithErr ${tp.show},  env=${env}")
        val retval = tp match
            case tpc: TermRef =>
                if tpc.typeSymbol.isTypeParam then unsupportedType(tp, s"TermRef ${tpc.show}", env)
                else if !(tpc.widen =:= tpc) then sirTypeInEnvWithErr(tp.widen, env)
                else unsupportedType(tp, s"TermRef ${tpc.show}", env)
            case tpc: TypeRef =>
                val sym = tpc.typeSymbol
                if tpc =:= defn.NothingType then SIRType.TypeNothing
                else if tpc.isTypeAlias then sirTypeInEnvWithErr(tpc.dealias, env)
                else if tpc =:= defn.AnyType then SIRType.FreeUnificator
                else if sym.isClass then makeSIRNonFunClassType(tpc, Nil, env)
                else if sym.isTypeParam then
                    env.vars.get(sym) match
                        case Some(t) => t
                        case None =>
                            val name = sym.showFullName
                            unsupportedType(tp, s"Unfilled typeParam: ${tpc.show}", env)
                else if sym.isAliasType then
                    // looks like bug in a compiler
                    // println(s"stragen alias: ${tpc.show}, tpc.isTypeAlias=${tpc.isTypeAlias}, tp.isTypeAlias=${tp.isTypeAlias} sym.isAlias=${sym.isAliasType}, tpc.isSingleton=${tpc.isSingleton}")
                    // println(s"strange aliase: tree=${tpc}, symFullName=${sym.fullName}, dealias=${tpc.dealias.show}, deaslias.fullName=${tpc.dealias.typeSymbol.fullName}")
                    // throw new Exception("alias type")
                    sirTypeInEnvWithErr(tpc.dealias, env)
                else if tpc.isValueType then
                    val wtpc = tpc.widen
                    if wtpc != tpc then sirTypeInEnvWithErr(wtpc, env)
                    else makeSIRNonFunValueType(tpc, Nil, env)
                else unsupportedType(tpc, "TypeRef", env)
            case tpc: ConstantType =>
                // hmm, widen should have taken care of this
                sirTypeInEnvWithErr(tpc.widen, env)
            case tpc: SuperType =>
                ???
            case tpc: RefinedType =>
//                println(s"RefinedType ${tpc.show}")
                if tpc <:< defn.PolyFunctionType then {
                    // then this is a type of applyMethod
                    val applyTpe = tpc.member(nme.apply).info
                    sirTypeInEnvWithErr(applyTpe, env)
                } else {
                    sirTypeInEnvWithErr(tpc.parent, env)
                }
            case tp: AppliedType =>
                if (tp.tycon.isRef(defn.MatchCaseClass)) then
                    unsupportedType(tp, "MatchCaseClass", env)
                else if (defn.isFunctionType(tp)) then makeSIRFunType(tp, env)
                else
                    tp.tycon match
                        case tpc: TypeRef =>
                            if tpc.isTypeAlias || tpc.symbol.isAliasType then
                                sirTypeInEnvWithErr(tpc.dealias.appliedTo(tp.args), env)
                            else makeSIRNonFunClassType(tpc, tp.args.map(sirTypeInEnv(_, env)), env)
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
                val paramName =
                    binder
                        .paramNames(tp.paramNum)
                        .show // TODO: better way to get the name as string
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
                val params = (tpp.paramNames zip tpp.paramRefs).map { (name, ref) =>
                    SIRType.TypeVar(name.show, Some(ref.typeSymbol.hashCode()))
                }
                SIRType.TypeLambda(params, sirTypeInEnvWithErr(tpp.resultType, env))
            case tpl: HKTypeLambda =>
                val params = (tpl.paramNames zip tpl.paramRefs).map { (name, ref) =>
                    SIRType.TypeVar(name.show, Some(ref.typeSymbol.hashCode()))
                }
                SIRType.TypeLambda(params, sirTypeInEnvWithErr(tpl.resType, env))
            case tpp: TypeBounds =>
                SIRType.FreeUnificator
            case NoPrefix =>
                unsupportedType(tp, "NoPrefix", env)
            // case tpf: FlexibleType =>
            //    sirTypeInEnv(tpf.underlying, env)
            case classInfo: ClassInfo =>
                makeSIRNonFunClassType(classInfo.appliedRef, Nil, env)
            case typeVar: TypeVar =>
                env.vars.get(typeVar.typeSymbol) match
                    case Some(t) => t
                    case None    =>
                        // this can be filled typeVar
                        typeVar.stripped match
                            case TypeParamRef(binder, idx) =>
                                val paramName = binder.paramNames(idx).show
                                // unsupportedType(tp, s"TypeVar ${typeVar.show}, name=${name}", env)
                                val symCode =
                                    if typeVar.typeSymbol == Symbols.NoSymbol then
                                        // (binder.typeSymbol.hashCode().toLong << 32) + idx
                                        binder.typeSymbol.hashCode() + idx
                                    else typeVar.typeSymbol.hashCode()
                                //  not sure, if typeVar,typeSymbol is exista and is unique.
                                // code as binding symbol ?
                                // TODO: make SymCode long to accept such encoding
                                SIRType.TypeVar(paramName, Some(symCode))
                            case other =>
                                // this is a filled typeVar, which can be substitutef
                                sirTypeInEnvWithErr(other, env)
            case other =>
                unsupportedType(tp, s"${tp.show}, tree=${tp}", env)
        retval

    def makeSIRClassTypeNoTypeArgs(tp: Type, env: SIRTypeEnv)(using Context): SIRType = {
        // println(s"makeSIRClassTypeNoTypeArgs ${tp.show}")
        if (defn.isFunctionType(tp)) then makeFunTypeLambda(tp)
        else makeSIRNonFunClassType(tp, Nil, env)
    }

    def makeSIRNonFunClassType(tp: Type, types: List[SIRType], env: SIRTypeEnv)(using
        Context
    ): SIRType = {
        val sym = tp.typeSymbol
        // println(s"makeSIRNonFumClassType ${sym.showFullName} ${types.map(_.show)}, isFunctionType=${defn.isFunctionType(tp)}")
        val retval = (tryMakePrimitivePrimitive(sym, types) orElse
            tryMakeBuildinType(sym, types, env) orElse
            tryMakeCaseClassOrCaseParent(sym, types, env) orElse
            tryMakeNonCaseModule(tp, sym, types, env)).getOrElse {
            val name = sym.showFullName
            val typeArgs = types.map(_.show)
            unsupportedType(
              tp,
              s"tree=${tp}, isClass=${sym.isClass} isAliasType=${sym.isAliasType}, info:${sym.info}",
              env
            )
        }
        // println("nakeSIRNonFunClassType return " + retval)
        retval
    }

    def makeSIRNonFunValueType(tpc: TypeRef, params: List[Type], env: SIRTypeEnv)(using
        Context
    ): SIRType = {
        val sym = tpc.typeSymbol
        if (sym == Symbols.requiredClass("scala.math.BigInt")) then SIRType.IntegerPrimitive
        else
        // this is a custom value type,  check hidden val
        if tpc.typeSymbol.isTerm && !tpc.typeSymbol.isType then
            // sone strange type, which should be a TermRef,  not TypeRef
            //  (error in dotty  ???)
            val termSym = tpc.typeSymbol.asTerm
            sirTypeInEnvWithErr(termSym.info, env)
        else
            val argss = sym.primaryConstructor.paramSymss.filter(_.exists(!_.isTypeParam)).flatten
            argss match
                case Nil =>
                    unsupportedType(tpc, "ValueType without fields", env)
                case head :: Nil =>
                    val headType = head.info
                    sirTypeInEnvWithErr(headType, env)
                case _ =>
                    unsupportedType(
                      tpc,
                      "ValueType with more that one argument to ptrimary constryctir",
                      env
                    )
    }

    def makeSIRFunType(tp: Type, env: SIRTypeEnv)(using Context): SIRType = {
        tp match
            case mt: MethodType =>
                makeSIRMethodType(mt, env)
            case AppliedType(tycon, args) =>
                if (defn.isFunctionType(tp)) then
                    val retval = makeFunctionClassType(
                      tycon.typeSymbol,
                      args.map(sirTypeInEnvWithErr(_, env)),
                      env
                    )
                    retval
                else unsupportedType(tp, "AppliedType as function", env)
            case _ =>
                ???
    }

    def findClassInAndType(andType: AndType)(using Context): Option[Type] = {
        ???
    }

    def tryMakePrimitivePrimitive(symbol: Symbol, tpArgs: List[SIRType])(using
        Context
    ): Option[SIRType] = {
        if !tpArgs.isEmpty then None
        else if symbol == defn.BooleanType.typeSymbol || symbol == defn.BoxedBooleanClass then
            Some(SIRType.BooleanPrimitive)
        else if (symbol == Symbols.requiredClass("scalus.builtin.ByteString")) then
            Some(SIRType.ByteString)
        else if (symbol == Symbols.requiredClass("scala.math.BigInt")) then
            Some(SIRType.IntegerPrimitive)
        else if symbol == defn.IntType.typeSymbol || symbol == defn.BoxedIntClass then
            Some(SIRType.IntegerPrimitive)
        else if symbol == defn.LongType.typeSymbol || symbol == defn.BoxedLongClass then
            Some(SIRType.IntegerPrimitive)
        else if symbol == defn.StringType.typeSymbol then Some(SIRType.StringPrimitive)
        else if symbol == defn.UnitClass then Some(SIRType.VoidPrimitive)
        else None
    }

    def tryMakeBuildinType(symbol: Symbol, tpArgs: List[SIRType], env: SIRTypeEnv)(using
        Context
    ): Option[SIRType] = {
        if (symbol == Symbols.requiredClass("scalus.builtin.Data")) then Some(SIRType.Data)
        else if (symbol == Symbols.requiredClass("scalus.builtin.List")) then
            tpArgs match
                case List(elemType) => Some(SIRType.List(elemType))
                case _ =>
                    val err = SIRType.TypeError(
                      s"List type should have one type argument, found ${tpArgs.length}",
                      null
                    )
                    Some(err)
        else if (symbol == Symbols.requiredClass("scalus.builtin.Pair")) then
            tpArgs match
                case List(a1, a2) => Some(SIRType.Pair(a1, a2))
                case _ =>
                    Some(
                      SIRType.TypeError(
                        s"Pair type should have two type arguments, found ${tpArgs.length}",
                        null
                      )
                    )
        else if (symbol == Symbols.requiredClass("scalus.builtin.BLS12_381_G1_Element")) then
            Some(SIRType.BLS12_381_G1_Element)
        else if (symbol == Symbols.requiredClass("scalus.builtin.BLS12_381_G2_Element")) then
            Some(SIRType.BLS12_381_G2_Element)
        else if (symbol == Symbols.requiredClass("scalus.builtin.BLS12_381_MlResult")) then
            Some(SIRType.BLS12_381_MlResult)
        else None
    }

    def makeFunctionClassType(symbol: Symbols.Symbol, list: List[SIRType], env: SIRTypeEnv)(using
        Context
    ): SIRType = {
        val args = list.init
        val res = list.last
        makeUnaryFun(args, res)
    }

    def tryMakeCaseClassOrCaseParent(typeSymbol: Symbol, tpArgs: List[SIRType], env: SIRTypeEnv)(
        using Context
    ): Option[SIRType] = {
        env.forwardRefs.get(typeSymbol) match
            case Some(proxy) =>
                Some(proxy)
            case None =>
                val proxy = new SIRType.TypeProxy(null)
                val retval = tryMakeCaseClassOrCaseParentTypeNoRec(
                  typeSymbol,
                  tpArgs,
                  env.copy(forwardRefs = env.forwardRefs.updated(typeSymbol, proxy)),
                  proxy
                )
                retval match
                    case Some(t) =>
                        proxy.ref = t
                        Some(t)
                    case None =>
                        None
    }

    private def flatSealedTraitHierarchy(
        top: Type,
        childrens: List[SIRType],
        env: SIRTypeEnv
    ): List[ConstrDecl] = {
        childrens.flatMap {
            case SIRType.CaseClass(constrDecl, _)  => Some(constrDecl)
            case SIRType.SumCaseClass(dataDecl, _) => dataDecl.constructors
            case SIRType.TypeProxy(proxy) =>
                if proxy == null then {
                    Nil
                } else {
                    flatSealedTraitHierarchy(top, List(proxy), env)
                }
            case ch @ SIRType.TypeLambda(params, tp) =>
                val msg = s"TypeLambda in sealed trait hierarchy: ${tp.show}"
                throw TypingException(top, env.pos, msg)
            case other =>
                val msg = s"Invalid type in sealed trait hierarchy:  ${other.show}"
                throw TypingException(top, env.pos, msg)
        }
    }

    /** case classes and symbols.
      * @param typeSymbol
      * @param tpArgs
      * @param x$3
      * @return
      */
    def tryMakeCaseClassOrCaseParentTypeNoRec(
        typeSymbol: Symbol,
        tpArgs: List[SIRType],
        env: SIRTypeEnv,
        thisProxy: SIRType.TypeProxy
    )(using Context): Option[SIRType] = {
        // println(s"tryMakeCaseClassOrCaseParentTypeNoRec ${typeSymbol.showFullName} ${tpArgs.map(_.show)}, isCase=${typeSymbol.flags.is(Flags.CaseClass)}, isEnum=${typeSymbol.flags.is(Flags.Enum)}, flags=${typeSymbol.flagsString}")
        // println(s"typeSymbol.isType=${typeSymbol.isType}, typeSymbol.isClass=${typeSymbol.isClass}, typeSymbol.isTerm=${typeSymbol.isTerm}")
        if typeSymbol.flags.is(Flags.Case) || typeSymbol.flags.is(Flags.Enum) then {
            // case class, can do constrdecl
            val name = typeSymbol.fullName.show
            // if name==""

            val (typeParamSymbols, paramSymbols) = typeSymbol.primaryConstructor.paramSymss match
                case List(args) =>
                    if args.isEmpty then (Nil, Nil)
                    else if (args.exists(_.isTerm)) then (Nil, args)
                    else if (args.exists(_.isType)) then (args, Nil)
                    else {
                        val msg =
                            s"Case class ${typeSymbol.showFullName} has strange primary constructor: ${args}"
                        thisProxy.ref = SIRType.TypeError(msg, null)
                        throw TypingException(typeSymbol.info, env.pos, msg)
                    }
                case List(frs, snd) =>
                    if frs.exists(_.isType) && snd.exists(_.isTerm) then (frs, snd)
                    else if frs.exists(_.isTerm) && snd.exists(_.isType) then (snd, frs)
                    else if frs.exists(_.isType) && snd.exists(_.isType) then
                        val msg =
                            s"Case class ${typeSymbol.showFullName} has primary constructor with two type parametes list"
                        thisProxy.ref = SIRType.TypeError(msg, null)
                        throw TypingException(typeSymbol.info, env.pos, msg)
                    else if frs.exists(_.isTerm) && snd.exists(_.isTerm) then
                        val msg =
                            s"Not supported ${typeSymbol.showFullName} has primary constructor with multiole parametes list"
                        thisProxy.ref = SIRType.TypeError(msg, null)
                        throw TypingException(typeSymbol.info, env.pos, msg)
                    else if frs.exists(_.isType) && snd.isEmpty then (frs, Nil)
                    else if frs.isEmpty && snd.exists(_.isType) then (Nil, snd)
                    else if frs.exists(_.isTerm) && snd.isEmpty then (Nil, frs)
                    else if frs.isEmpty && snd.exists(_.isTerm) then (snd, Nil)
                    else {
                        val msg =
                            s"Case class ${typeSymbol.showFullName} has strange primary constructor: ${frs} ${snd}"
                        thisProxy.ref = SIRType.TypeError(msg, null)
                        throw TypingException(typeSymbol.info, env.pos, msg)
                    }
                case List(frs, snd, thr) =>
                    if frs.exists(_.isType) && snd.exists(_.isTerm) && thr.isEmpty then (frs, snd)
                    else
                        val msg =
                            s"Not supported ${typeSymbol.showFullName} has primary constructor with multiole parametes list"
                        thisProxy.ref = SIRType.TypeError(msg, null)
                        throw TypingException(typeSymbol.info, env.pos, msg)
                case _ =>
                    val msg =
                        s"Case class ${typeSymbol.showFullName} has primary constructor with multiply parameters list: ${typeSymbol.primaryConstructor.paramSymss}"
                    thisProxy.ref = SIRType.TypeError(msg, null)
                    throw TypingException(typeSymbol.info, env.pos, msg)

            val tparams = typeParamSymbols.map(s => SIRType.TypeVar(s.name.show, Some(s.hashCode)))
            val nVars = typeParamSymbols.zip(tparams).foldLeft(env.vars) {
                case (acc, (sym, tvar)) => acc.updated(sym, tvar)
            }
            val nEnv = env.copy(vars = nVars)
            // val params1 = typeSymbol.info.fields
            //    .map(f => TypeBinding(f.name.show, sirTypeInEnv(f.info, nEnv)))
            //    .toList
            val params = paramSymbols
                .map(s =>
                    val t = sirTypeInEnv(s.info, nEnv)
                    // println(s"param ${s.show} -> ${t.show}")
                    TypeBinding(s.name.show, t)
                )
                .toList
            // TODO:  get 'most top?'
            val optBaseSymbol = typeSymbol.info.baseClasses.find(bc => bc.children.nonEmpty)
            val constrDecl = optBaseSymbol match
                case Some(baseSymbol) =>
                    // val baseClassType = typeSymbol.info.baseType(baseSymbol)
                    // val parentTParams = baseClassType match
                    //    case AppliedType(tycon, args) => args.map(sirTypeInEnv(_, nEnv))
                    //    case _ => Nil
                    // ConstrDecl(name, SIRVarStorage.Data, params, tparams, parentTParams)
                    ConstrDecl(name, SIRVarStorage.Data, params, tparams)
                case None =>
                    // ConstrDecl(name, SIRVarStorage.Data, params, tparams, Nil)
                    ConstrDecl(name, SIRVarStorage.Data, params, tparams)
            val nType = SIRType.CaseClass(constrDecl, tpArgs)
            Some(nType)
        } else {
            // TODO: keep in env mapSymbol => SumCaseClass to prevent duplication
            if typeSymbol.children.nonEmpty then {
                val childrenSymbols = typeSymbol.children
                val childrenTypes = childrenSymbols.map(s =>
                    val typeParams = s.primaryConstructor.typeParams.map(s =>
                        SIRType.TypeVar(s.name.show, Some(s.hashCode))
                    )
                    val nVars = env.vars ++ s.primaryConstructor.typeParams.zip(typeParams)
                    val nEnv = env.copy(vars = nVars)
                    // val parentTpArgs = s.info.baseType(typeSymbol) match
                    //        case AppliedType(tycon, args) => args.map(sirTypeInEnv(_, nEnv))
                    //        case _ => Nil
                    if s.children.isEmpty then
                        // val constrDecl = ConstrDecl(s.name.show, SIRVarStorage.DEFAULT, Nil, typeParams, parentTpArgs)
                        val constrDecl =
                            ConstrDecl(s.name.show, SIRVarStorage.DEFAULT, Nil, typeParams)

                        SIRType.CaseClass(constrDecl, typeParams)
                    else
                        val proxy = new SIRType.TypeProxy(null)
                        val retval =
                            tryMakeCaseClassOrCaseParentTypeNoRec(s, typeParams, nEnv, proxy)
                        retval match
                            case Some(t) =>
                                proxy.ref = t
                                t
                            case None =>
                                val msg =
                                    s"Case parent type ${typeSymbol.showFullName} has children that are not case classes or case parent types: ${s.show}"
                                thisProxy.ref = SIRType.TypeError(msg, null)
                                throw TypingException(typeSymbol.info, env.pos, msg)
                )
                childrenSymbols.zip(childrenTypes).find { case (sym, chtp) =>
                    !chtp.isInstanceOf[SIRType.CaseClass] &&
                    !chtp.isInstanceOf[SIRType.SumCaseClass] &&
                    !chtp.isInstanceOf[SIRType.TypeLambda] &&
                    !chtp.isInstanceOf[SIRType.TypeProxy]
                } match
                    case Some((childSym, childStrangeType)) =>
                        childStrangeType match
                            case err @ SIRType.TypeError(msg, cause) =>
                                // return typeError. Note, that if we here then throwError already set to false.
                                Some(
                                  typeError(
                                    typeSymbol.info,
                                    msg,
                                    env,
                                    throwError = false,
                                    cause = cause
                                  )
                                )
                            case _ =>
                                val msg =
                                    s"Case parent type ${typeSymbol.showFullName} has children that are not case classes or case parent types: ${childSym.showFullName}: ${childStrangeType}"
                                thisProxy.ref = SIRType.TypeError(msg, null)
                                Some(typeError(typeSymbol.info, msg, env, throwError = true))
                    case None =>
                        val name = typeSymbol.fullName.show
                        val tparams = typeSymbol.info.typeParamSymbols.map(s =>
                            SIRType.TypeVar(s.name.show, Some(s.hashCode))
                        )
                        if tparams.length != tpArgs.length then {
                            // println(s"Children types: ${childrenTypes}")
                            // println(s"Children symbols: ${childrenSymbols.map(_.showFullName)}")
                            val msg =
                                s"Case parent type ${typeSymbol.showFullName} has ${tparams.length} type parameters, but ${tpArgs.length} were provided"
                            thisProxy.ref = SIRType.TypeError(msg, null)
                            Some(typeError(typeSymbol.info, msg, env, throwError = true))
                        } else {
                            val constrDecls =
                                flatSealedTraitHierarchy(typeSymbol.info, childrenTypes, env)
                            val nType =
                                SIRType.SumCaseClass(DataDecl(name, constrDecls, tparams), tpArgs)
                            thisProxy.ref = nType
                            Some(nType)
                        }
            } else {
                None
            }
        }
    }

    def tryMakeNonCaseModule(tp: Type, typeSymbol: Symbol, tpArgs: List[SIRType], env: SIRTypeEnv)(
        using Context
    ): Option[SIRType] = {
        if typeSymbol.flags.is(Flags.Module) || typeSymbol.flags.is(Flags.Package) then {
            val name = typeSymbol.fullName.show
            if tpArgs.nonEmpty then {
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
            case Nil          => res
            case head :: Nil  => SIRType.Fun(head, res)
            case head :: tail => SIRType.Fun(head, makeUnaryFun(tail, res))
    }

    def makeSIRMethodType(mt: MethodType, env: SIRTypeEnv)(using Context): SIRType = {
        val params = mt.paramNames.zip(mt.paramInfos).map { (_, tp) =>
            sirTypeInEnvWithErr(tp, env)
        }
        val res = sirTypeInEnvWithErr(mt.resultType, env)
        makeUnaryFun(params, res)
    }

    def makeFunTypeLambda(fn: Type): SIRType = ???

    def typeError(
        tpe: Type,
        msg: String,
        env: SIRTypeEnv,
        throwError: Boolean = true,
        cause: Throwable = null
    )(using Context): SIRType = {
        if throwError then throw TypingException(tpe, env.pos, msg)
        else SIRType.TypeError(msg, null)
    }

    def unsupportedType(tpe: Type, msg: String, env: SIRTypeEnv, throwError: Boolean = true)(using
        Context
    ): SIRType = {
        typeError(tpe, s"unsupported type: ${tpe.show} $msg", env, throwError)
    }

}
