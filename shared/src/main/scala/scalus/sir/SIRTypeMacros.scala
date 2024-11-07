package scalus.sir

import scala.quoted.*
import scala.util.control.NonFatal



object SIRTypeMacros {
    import SIRType.*;

    inline def liftM[T <: AnyKind]: SIRType = ${ liftMImpl[T] }


    def liftMImpl[T <: AnyKind : Type](using Quotes): Expr[SIRType] = {
        import quotes.reflect.*
        
        case class Env(
                          val forwardRefs: Map[Symbol, TypeProxy] = Map.empty,
                          val vars: Map[Symbol, TypeVar] = Map.empty,
                          filledVars: Map[TypeVar, SIRType] = Map.empty,
                          //var enclosingBounds = Map.empty[Int,TypeVar]
                      )

        def liftRepr(tp: TypeRepr, env: Env): SIRType = {
            tp match
                case typeLambda@quotes.reflect.TypeLambda(paramNames, paramBounds, resType) =>
                    //println(s"type-lambda detected, tp.symbol = ${tp.typeSymbol}, tp.symbol.hashCode=${tp.typeSymbol.hashCode}, Type: ${tp.show}")
                    // we need to generate param names with unique ids.
                    var newVars = env.vars
                    val sirParamExprs = paramNames.zipWithIndex.map {
                        case (name, idx) =>
                            val sym = typeLambda.param(idx).typeSymbol
                            val id = sym.hashCode
                            val tv = TypeVar(name, Some(id))
                            newVars = newVars + (sym -> tv)
                            tv
                    }
                    val nEnv = env.copy(vars = newVars)
                    SIRType.TypeLambda(sirParamExprs, liftRepr(resType, nEnv))
                case pr@quotes.reflect.ParamRef(p, n) =>
                    //println(s"ParamRef detected,  Type: ${tp.show}, p=$p, p.typeSymbol.hashCode = ${p.typeSymbol.hashCode} n=$n")
                    val name = tp.show // TODO: get from enclosing lambda.
                    val id = pr.typeSymbol.hashCode
                    env.vars.get(pr.typeSymbol) match
                        case Some(value) =>
                        case None =>
                            report.error(s"No enclosing lambda found for typevar ${tp.show}", Position.ofMacroExpansion)
                    TypeVar(name, Some(id))
                case tr: TermRef =>
                    if !(tr.widen =:= tr) then
                        liftRepr(tp.widen, env)
                    else
                        report.error(s"TermRef is unsupported,  Type: ${tp.show}", Position.ofMacroExpansion)
                        SIRType.TypeError(s"TermRef ${tp.show} is unsupported", null)
                case tpc: TypeRef =>
                    val sym = tpc.typeSymbol
                    if (sym == defn.NothingClass) then
                        SIRType.TypeNothing
                    else if (sym.isAliasType) then
                        liftRepr(tpc.dealias, env)
                    else if (sym.isClassDef) then
                        liftClassType(tpc, Nil, env)
                    else if (sym.isTypeParam) then
                        env.vars.get(sym) match
                            case Some(tv) => tv
                            case None =>
                                report.error(s"Unbound type variable ${tpc.show}", Position.ofMacroExpansion)
                                SIRType.TypeError(s"Unbound type variable ${tpc.show}", null)
                    else
                        report.error(s"Unsupported type [1]: ${tp.show}", Position.ofMacroExpansion)
                        SIRType.TypeError(s"Unsupported type [1]: ${tp.show}", null)
                case ct: ConstantType =>
                    liftRepr(tp.widen, env)
                case other =>
                    tp.asType match {
                        case '[scalus.builtin.ByteString] =>
                            ByteStringPrimitive
                        case '[BigInt] =>
                            IntegerPrimitive
                        case '[String] => StringPrimitive
                        case '[Boolean] => BooleanPrimitive
                        case '[Unit] => VoidPrimitive
                        case '[scalus.builtin.Data] => Data
                        case '[a => b] =>
                            //println(s"Fun detected,  Type: ${tp.show}")
                            val in = TypeRepr.of[a]
                            val out = TypeRepr.of[b]
                            Fun(liftRepr(in, env), liftRepr(out, env))
                        case '[(a, b) => c] =>
                            println(s"Fun2 detected,  Type: ${tp.show}")
                            //report.error(s"Uncarried function types are not supported: ${tp.show}", Position.ofMacroExpansion)
                            Fun(liftRepr(TypeRepr.of[a], env),
                                Fun(liftRepr(TypeRepr.of[b], env),
                                    liftRepr(TypeRepr.of[c], env)))
                        case '[(a, b, c) => d] =>
                            //println(s"Fun3 detected,  Type: ${tp.show}")
                            val ta = TypeRepr.of[a]
                            val tb = TypeRepr.of[b]
                            val tc = TypeRepr.of[c]
                            val td = TypeRepr.of[d]
                            //println(s"ta: ${ta.show}, tb: ${tb.show}, tc: ${tc.show}, td: ${td.show}")
                            //report.error(s"Uncarried function types are not supported: ${tp.show}", Position.ofMacroExpansion)
                            Fun(liftRepr(TypeRepr.of[a], env),
                                Fun(liftRepr(TypeRepr.of[b], env),
                                    Fun(liftRepr(TypeRepr.of[c], env),
                                        liftRepr(TypeRepr.of[d], env))))
                        case '[(a, b, c, d) => e] =>
                            //println(s"Fun4 detected,  Type: ${tp.show}")
                            Fun(liftRepr(TypeRepr.of[a], env),
                                Fun(liftRepr(TypeRepr.of[b], env),
                                    Fun(liftRepr(TypeRepr.of[c], env),
                                        Fun(liftRepr(TypeRepr.of[d], env),
                                            liftRepr(TypeRepr.of[e], env)))))
                        case '[(a, b, c, d, e) => f] =>
                            //println(s"Fun5 detected,  Type: ${tp.show}")
                            Fun(liftRepr(TypeRepr.of[a], env),
                                Fun(liftRepr(TypeRepr.of[b], env),
                                    Fun(liftRepr(TypeRepr.of[c], env),
                                        Fun(liftRepr(TypeRepr.of[d], env),
                                            Fun(liftRepr(TypeRepr.of[e], env),
                                                liftRepr(TypeRepr.of[f], env))))))
                        case '[(a, b, c, d, e, f) => g] =>
                            //println(s"Fun6 detected,  Type: ${tp.show}")
                            Fun(liftRepr(TypeRepr.of[a], env),
                                Fun(liftRepr(TypeRepr.of[b], env),
                                    Fun(liftRepr(TypeRepr.of[c], env),
                                        Fun(liftRepr(TypeRepr.of[d], env),
                                            Fun(liftRepr(TypeRepr.of[e], env),
                                                Fun(liftRepr(TypeRepr.of[f], env),
                                                    liftRepr(TypeRepr.of[g], env)))))))
                        case '[scalus.builtin.Pair[a, b]] =>
                            //println(s"Builtin pair detected,  Type: ${tp.show}")
                            val a = TypeRepr.of[a]
                            val b = TypeRepr.of[b]
                            SIRType.Pair(liftRepr(a, env),
                                liftRepr(b, env))
                        case '[(a, b)] =>
                            //println(s"Tuple detected,  Type: ${tp.show}")
                            val a = TypeRepr.of[a]
                            val b = TypeRepr.of[b]
                            SIRType.Pair(liftRepr(a, env),
                                liftRepr(b, env))
                        case '[scalus.builtin.List[a]] =>
                            //println(s"List detected,  Type: ${tp.show}")
                            val a = TypeRepr.of[a]
                            SIRType.List(liftRepr(a, env))
                        case '[Option[a]] =>
                            ???
                        case other =>
                            println(s"Unrecognized type: ${tp.show} tree: ${other}, tp=${tp}")
                            report.error(s"Unsupported type [1]: ${tp.show} in ${TypeRepr.of[T].show}", Position.ofMacroExpansion)
                            ???
                    }
        }

        def liftClassType(tp: TypeRepr, typeArgs: List[SIRType], env: Env): SIRType = {
            val sym = tp.typeSymbol
            val retval = (tryLiftPrimitive(sym, typeArgs) orElse
                tryLiftBuildinType(sym, typeArgs, env) orElse
                tryLiftCaseClassOrCaseParent(tp, sym, typeArgs, env) orElse
                tryMakeNonCaseModule(tp, sym, typeArgs, env)
                ).getOrElse {
                val name = sym.name
                val typeArgsShow = typeArgs.map(_.show)
                unsupportedType(tp, s"tree=${tp}, isClass=${sym.isClassDef} isAliasType=${sym.isAliasType}", env)
            }
            retval
        }

        def tryLiftPrimitive(sym: Symbol, types: List[SIRType]): Option[SIRType] = {
            if (sym == defn.IntClass || sym == defn.LongClass) then
                Some(IntegerPrimitive)
            else if (sym == Symbol.requiredClass("scalus.builtin.ByteString")) then
                Some(ByteStringPrimitive)
            else if (sym == Symbol.requiredClass("scala.math.BigInt")) then
                Some(IntegerPrimitive)
            else if (sym == defn.StringClass) then
                Some(StringPrimitive)
            else if (sym == defn.BooleanClass) then
                Some(BooleanPrimitive)
            else if (sym == defn.UnitClass) then
                Some(VoidPrimitive)
            else
                None
        }

        def tryLiftBuildinType(sym: Symbol, types: List[SIRType], env: Env): Option[SIRType] = {
            if (sym == Symbol.requiredClass("scalus.builtin.Data")) then
                Some(Data)
            else if (sym == Symbol.requiredClass("scalus.builtin.List")) then
                Some(SIRType.List(types.head))
            else if (sym == Symbol.requiredClass("scalus.builtin.Pair")) then
                Some(SIRType.Pair(types.head, types(1)))
            else
                None
        }

        def tryLiftCaseClassOrCaseParent(tp: TypeRepr, sym: Symbol, targs: List[SIRType], env: Env): Option[SIRType] = {
            env.forwardRefs.get(sym) match
                case Some(tp) =>
                    Some(tp)
                case None =>
                    val TypeProxy = new TypeProxy(null)
                    val nEnv = env.copy(forwardRefs = env.forwardRefs + (sym -> TypeProxy))
                    tryLiftCaseClassOrCaseParentNoProxy(tp, sym, targs, nEnv) match
                        case Some(sirType) =>
                            TypeProxy.ref = sirType
                            Some(sirType)
                        case None =>
                            None
        }

        def tryLiftCaseClassOrCaseParentNoProxy(tp: TypeRepr, symbol: Symbol, targs: List[SIRType], env: Env): Option[SIRType] = {
            if (symbol.flags.is(Flags.Case) || symbol.flags.is(Flags.Enum)) {
                ???
            } else {
                ???
            }
        }

        def tryMakeNonCaseModule(tp: TypeRepr, symbol: Symbol, targs: List[SIRType], env: Env): Option[SIRType] = {
            ???
        }

        def unsupportedType(tp: TypeRepr, msg: String, env: Env, throwException: Boolean = true): SIRType = {
            val fullMessage = s"Unsupported type: ${tp.show} in ${TypeRepr.of[T].show}, $msg"
            report.error(fullMessage, Position.ofMacroExpansion)
            if (throwException) then
                throw new IllegalArgumentException(fullMessage)
            SIRType.TypeError(fullMessage, null)
        }

        try
            val sirType = liftRepr(TypeRepr.of[T], Env())
            SIRTypeToExpr.apply(sirType)
        catch
            case NonFatal(ex) =>
                println(s"Error: during lifting ${TypeRepr.of[T].show}: ${ex.getMessage}")
                ex.printStackTrace()
                throw ex
    }


}
