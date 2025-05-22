package scalus.sir
import scalus.sir.SIR.Pattern

object SIRUnify {

    case class Env(
        path: List[String] = List.empty,
        filledTypes: Map[SIRType.TypeVar, SIRType] = Map.empty,
        eqTypes: Map[SIRType.TypeVar, SIRType] = Map.empty,
        parentTypes: Map[SIRType, Set[SIRType]] = Map.empty,
        debug: Boolean = false
    )

    object Env {
        def empty: Env = Env()
    }

    sealed trait UnificationResult[+T] {

        def isSuccess: Boolean = this match {
            case UnificationSuccess(_, _)    => true
            case UnificationFailure(_, _, _) => false
        }

        def isFailure: Boolean = !isSuccess

        def map[U](f: T => U): UnificationResult[U] = this match {
            case UnificationSuccess(env, unificator)   => UnificationSuccess(env, f(unificator))
            case UnificationFailure(path, left, right) => UnificationFailure(path, left, right)
        }

        def flatMap[U](f: T => UnificationResult[U]): UnificationResult[U] = this match {
            case UnificationSuccess(env, unificator)   => f(unificator)
            case UnificationFailure(path, left, right) => UnificationFailure(path, left, right)
        }

    }

    object UnificationResult {

        def pure[A](a: A): UnificationResult[A] = UnificationSuccess(Env.empty, a)

    }

    case class UnificationSuccess[T](env: Env, unificator: T) extends UnificationResult[T]
    case class UnificationFailure[S](path: List[String], left: S, right: S)
        extends UnificationResult[Nothing]

    trait Unify[T] {
        def apply(left: T, right: T, env: Env): UnificationResult[T]
    }

    object Unify {

        given Unify[String] with {
            def apply(left: String, right: String, env: Env): UnificationResult[String] = {
                if left == right then UnificationSuccess(env, left)
                else UnificationFailure(env.path, left, right)
            }
        }

    }

    extension [T: Unify](left: T)
        def ~=~(right: T): UnificationResult[T] = summon[Unify[T]].apply(left, right, Env.empty)
        def ~~=~~(right: T): Boolean =
            summon[Unify[T]].apply(left, right, Env(debug = true)).isSuccess

    def unifySIR(left: SIR, right: SIR, env: Env): UnificationResult[SIR] = {
        unifySIRExpr(left, right, env)
        // (left, right) match
        //    case (expr1: SIRExpr, expr2: SIRExpr) =>
        //         unifySIRExpr(expr1, expr2, env)
        //    case (expr1: SIRExpr, decl: SIR.Decl) =>
        //        UnificationFailure(env.path, left, right)
        //    case (decl: SIR.Decl, expr2: SIRExpr) =>
        //        UnificationFailure(env.path, left, right)
        //    case (decl1: SIR.Decl, decl2: SIR.Decl) =>
        //        unifyDecl(decl1, decl2, env)
    }

    /** Unify two SIR expressions. Annotations are ignored during unification and can be different.
      */
    given Unify[SIR] with {
        def apply(left: SIR, right: SIR, env: Env): UnificationResult[SIR] = {
            unifySIRExpr(left, right, env)
        }
    }

    def unifySIRExpr(left: SIR, right: SIR, env: Env): UnificationResult[SIR] = {
        (left, right) match
            case (SIR.Var(name1, tp1, anns1), SIR.Var(name2, tp2, anns2)) =>
                if env.debug then println(s"unifySIRExpr: vars: \nleft=$left\nright=$right")
                if name1 == name2 then
                    unifyType(tp1, tp2, env.copy(path = "tp" :: env.path)) match
                        case UnificationSuccess(env1, tp) =>
                            UnificationSuccess(
                              env1.copy(path = env.path, parentTypes = Map.empty),
                              SIR.Var(name1, tp, anns1)
                            )
                        case UnificationFailure(path, tpLeft, tpRight) =>
                            UnificationFailure(path, left, right)
                else UnificationFailure("name" :: env.path, left, right)
            case (v1: SIR.ExternalVar, v2: SIR.ExternalVar) =>
                if v1.name == v2.name && v1.moduleName == v2.moduleName then
                    unifyType(v1.tp, v2.tp, env.copy(path = "tp" :: env.path)) match
                        case UnificationSuccess(env1, tp) =>
                            UnificationSuccess(
                              env1.copy(path = env.path, parentTypes = Map.empty),
                              SIR.ExternalVar(v1.name, v1.moduleName, tp, v1.anns)
                            )
                        case UnificationFailure(path, tpLeft, tpRight) =>
                            UnificationFailure(path, left, right)
                else UnificationFailure(env.path, left, right)
            case (v1: SIR.Let, v2: SIR.Let) =>
                unifyList(
                  v1.bindings,
                  v2.bindings,
                  env.copy(path = "binding" :: env.path)
                ) match
                    case UnificationSuccess(env1, bindings) =>
                        unifySIRExpr(
                          v1.body,
                          v2.body,
                          env1.copy(path = "body" :: env.path)
                        ) match
                            case UnificationSuccess(env2, body) =>
                                val recursivity =
                                    if v1.recursivity == v2.recursivity then v1.recursivity
                                    else
                                        // TODO: print warning ?
                                        Recursivity.NonRec
                                UnificationSuccess(
                                  env2,
                                  SIR.Let(recursivity, bindings, body, v1.anns)
                                )
                            case UnificationFailure(path, bodyLeft, bodyRight) =>
                                UnificationFailure(path, bodyLeft, bodyRight)
                    case UnificationFailure(path, bindingsLeft, bindingsRight) =>
                        UnificationFailure(path, bindingsLeft, bindingsRight)
            case (v1: SIR.LamAbs, v2: SIR.LamAbs) =>
                // note, that this is not semantic unification, but syntactic
                //  so, different parametes means different lambda.
                unifySIRExpr(v1.param, v2.param, env.copy()) match
                    case UnificationSuccess(env1, param) =>
                        val nParam = param.asInstanceOf[SIR.Var]
                        unifySIRExpr(v1.term, v2.term, env1.copy(path = "body" :: env.path)) match
                            case UnificationSuccess(env2, term) =>
                                UnificationSuccess(
                                  env2.copy(path = env.path),
                                  SIR.LamAbs(nParam, term, v1.anns)
                                )
                            case failure @ UnificationFailure(path, bodyLeft, bodyRight) =>
                                failure
                    case failure @ UnificationFailure(path, paramLeft, paramRight) =>
                        failure
            case (app1: SIR.Apply, app2: SIR.Apply) =>
                unifySIR(app1.f, app2.f, env.copy(path = "f" :: env.path)) match
                    case UnificationSuccess(env1, fun) =>
                        unifySIR(app1.arg, app2.arg, env1.copy(path = "arg" :: env.path)) match
                            case UnificationSuccess(env2, arg) =>
                                unifyType(
                                  app1.tp,
                                  app2.tp,
                                  env2.copy(path = "tp" :: env.path)
                                ) match
                                    case UnificationSuccess(env3, tp) =>
                                        UnificationSuccess(
                                          env3.copy(path = env.path, parentTypes = Map.empty),
                                          SIR.Apply(fun, arg, tp, app1.anns)
                                        )
                                    case UnificationFailure(path, tpLeft, tpRight) =>
                                        UnificationFailure(path, left, right)
                            case failure @ UnificationFailure(path, argLeft, argRight) => failure
                    case failure @ UnificationFailure(path, funLeft, funRight) => failure
            case (s1: SIR.Select, s2: SIR.Select) =>
                unifySIR(s1.scrutinee, s2.scrutinee, env.copy(path = "scrutinee" :: env.path)) match
                    case UnificationSuccess(env1, scrutinee) =>
                        if s1.field == s2.field then
                            unifyType(s1.tp, s2.tp, env.copy(path = "tp" :: env.path)) match
                                case UnificationSuccess(env2, tp) =>
                                    UnificationSuccess(
                                      env1,
                                      SIR.Select(scrutinee, s1.field, s1.tp, s1.anns)
                                    )
                                case UnificationFailure(path, tpLeft, tpRight) =>
                                    UnificationFailure(path, left, right)
                        else UnificationFailure(env.path, left, right)
                    case failure @ UnificationFailure(path, leftLeft, leftRight) => failure
            case (c1: SIR.Const, c2: SIR.Const) =>
                if c1.uplcConst == c2.uplcConst then UnificationSuccess(env, c1)
                else UnificationFailure(env.path, left, right)
            case (v1: SIR.And, v2: SIR.And) =>
                unifySIR(v1.a, v2.a, env.copy(path = "a" :: env.path)) match
                    case UnificationSuccess(env1, a) =>
                        unifySIR(v1.b, v2.b, env1.copy(path = "b" :: env.path)) match
                            case UnificationSuccess(env2, b) =>
                                UnificationSuccess(env2, SIR.And(a, b, v1.anns))
                            case failure @ UnificationFailure(path, rightLeft, rightRight) =>
                                failure
                    case failure @ UnificationFailure(path, leftLeft, leftRight) => failure
            case (v1: SIR.Or, v2: SIR.Or) =>
                unifySIR(v1.a, v2.a, env.copy(path = "a" :: env.path)) match
                    case UnificationSuccess(env1, a) =>
                        unifySIR(v1.b, v2.b, env1.copy(path = "b" :: env.path)) match
                            case UnificationSuccess(env2, b) =>
                                UnificationSuccess(env2, SIR.Or(a, b, v1.anns))
                            case failure: UnificationFailure[?] => failure
                    case failure: UnificationFailure[?] => failure
            case (v1: SIR.Not, v2: SIR.Not) =>
                unifySIR(v1.a, v2.a, env.copy(path = "a" :: env.path)) match
                    case UnificationSuccess(env1, a) =>
                        UnificationSuccess(env1.copy(path = env.path), SIR.Not(a, v1.anns))
                    case failure: UnificationFailure[?] => failure
            case (v1: SIR.IfThenElse, v2: SIR.IfThenElse) =>
                unifySIR(v1.cond, v2.cond, env.copy(path = "cond" :: env.path)) match
                    case UnificationSuccess(env1, cond) =>
                        unifySIR(v1.t, v2.t, env1.copy(path = "t" :: env.path)) match
                            case UnificationSuccess(env2, t) =>
                                unifySIR(v1.f, v2.f, env2.copy(path = "f" :: env.path)) match
                                    case UnificationSuccess(env3, f) =>
                                        unifyType(
                                          v1.tp,
                                          v2.tp,
                                          env3.copy(path = "tp" :: env.path)
                                        ) match
                                            case UnificationSuccess(env4, tp) =>
                                                UnificationSuccess(
                                                  env4.copy(
                                                    path = env.path,
                                                    parentTypes = Map.empty
                                                  ),
                                                  SIR.IfThenElse(cond, t, f, tp, v1.anns)
                                                )
                                            case failure @ UnificationFailure(path, left, right) =>
                                                failure
                                    case failure @ UnificationFailure(
                                          path,
                                          rightLeft,
                                          rightRight
                                        ) =>
                                        failure
                            case failure @ UnificationFailure(path, rightLeft, rightRight) =>
                                failure
                    case failure @ UnificationFailure(path, leftLeft, leftRight) => failure
            case (v1: SIR.Builtin, v2: SIR.Builtin) =>
                if v1.bn == v2.bn then UnificationSuccess(env, v1)
                else UnificationFailure(env.path, left, right)
            case (e1: SIR.Error, e2: SIR.Error) =>
                if e1.msg == e2.msg then UnificationSuccess(env, e1)
                else UnificationFailure(env.path, left, right)
            case (c1: SIR.Constr, c2: SIR.Constr) =>
                if c1.name == c2.name then
                    if c1.args.length != c2.args.length then
                        UnificationFailure(env.path, left, right)
                    else
                        val nEnv = env.copy(path = "args" :: env.path)
                        unifyList(c1.args, c2.args, nEnv) match
                            case UnificationSuccess(env1, args) =>
                                unifyDataDecl(
                                  c1.data,
                                  c2.data,
                                  env1.copy(path = "data" :: env.path)
                                ) match
                                    case UnificationSuccess(env2, data) =>
                                        UnificationSuccess(
                                          env2.copy(path = env.path),
                                          SIR.Constr(c1.name, data, args, c1.tp, c1.anns)
                                        )
                                    case failure @ UnificationFailure(path, left, right) =>
                                        failure
                            case failure @ UnificationFailure(path, left, right) => failure
                else UnificationFailure(env.path, left, right)
            case (m1: SIR.Match, m2: SIR.Match) =>
                unifySIR(m1.scrutinee, m2.scrutinee, env.copy(path = "scrutinee" :: env.path)) match
                    case UnificationSuccess(env1, expr) =>
                        unifyCases(m1.cases, m2.cases, env1.copy(path = "cases" :: env.path)) match
                            case UnificationSuccess(env2, cases) =>
                                unifyType(m1.tp, m2.tp, env2.copy(path = "tp" :: env.path)) match
                                    case UnificationSuccess(env3, tp) =>
                                        UnificationSuccess(
                                          env3.copy(path = env.path),
                                          SIR.Match(expr, cases, tp, m1.anns)
                                        )
                                    case failure @ UnificationFailure(path, left, right) =>
                                        failure
                            case failure @ UnificationFailure(path, left, right) => failure
                    case failure @ UnificationFailure(path, left, right) => failure
            case (d1: SIR.Decl, d2: SIR.Decl) =>
                unifyDataDecl(d1.data, d2.data, env.copy(path = "data" :: env.path)) match
                    case UnificationSuccess(env1, data) =>
                        unifySIR(d1.term, d2.term, env1.copy(path = "term" :: env.path)) match
                            case UnificationSuccess(env2, term) =>
                                UnificationSuccess(env2.copy(path = env.path), SIR.Decl(data, term))
                            case failure @ UnificationFailure(path, left, right) => failure
                    case failure @ UnificationFailure(path, left, right) => failure
            case _ =>
                UnificationFailure(env.path, left, right)
    }

    def checkEqType(env: Env, k: SIRType.TypeVar, v: SIRType): UnificationResult[SIRType] = {
        env.eqTypes.get(k) match {
            case Some(t2) =>
                val nEqTypes = env.eqTypes - k
                unifyType(t2, v, env.copy(eqTypes = nEqTypes))
            case None =>
                UnificationSuccess(env, v)
        }
    }

    given Unify[SIRType] with {
        def apply(left: SIRType, right: SIRType, env: Env): UnificationResult[SIRType] = {
            unifyType(left, right, env)
        }
    }

    def unifyType(left: SIRType, right: SIRType, env: Env): UnificationResult[SIRType] = {
        if env.debug then println(s"unifyType: \nleft=$left\nright=$right")
        val retval =
            if left eq right then UnificationSuccess(env, left)
            else
                env.parentTypes.get(left) match
                    case Some(parentsRight) =>
                        if parentsRight.contains(right) then
                            // Too optimistics ?
                            //
                            UnificationSuccess(env, right)
                        else
                            val nEnv = env.copy(parentTypes =
                                env.parentTypes.updated(left, parentsRight + right)
                            )
                            unifyTypeNoRec(left, right, nEnv)
                    case None =>
                        val nEnv = env.copy(parentTypes = env.parentTypes.updated(left, Set(right)))
                        unifyTypeNoRec(left, right, nEnv)
        if env.debug then println(s"return unifyType\nleft=$left\nright=$right\nretval=$retval")
        retval
    }

    def unifyTypeNoRec(left: SIRType, right: SIRType, env: Env): UnificationResult[SIRType] = {
        (left, right) match
            case (v1: SIRType.TypeVar, v2: SIRType.TypeVar) =>
                env.filledTypes.get(v1) match
                    case Some(tp1) =>
                        env.filledTypes.get(v2) match
                            case Some(tp2) =>
                                unifyType(tp1, tp2, env)
                            case None =>
                                val nFilledTypes = env.filledTypes.updated(v2, tp1)
                                checkEqType(env.copy(filledTypes = nFilledTypes), v2, tp1)
                    case None =>
                        env.filledTypes.get(v2) match
                            case Some(tp2) =>
                                val nFilledTypes = env.filledTypes.updated(v1, tp2)
                                checkEqType(env.copy(filledTypes = nFilledTypes), v1, tp2)
                            case None =>
                                val nEqTypes = env.eqTypes.updated(v1, v2)
                                UnificationSuccess(env.copy(eqTypes = nEqTypes), v1)
            case (v: SIRType.TypeVar, _) =>
                val nEnv = env.copy(filledTypes = env.filledTypes.updated(v, right))
                checkEqType(nEnv, v, right)
            case (_, v: SIRType.TypeVar) =>
                val nEnv = env.copy(filledTypes = env.filledTypes.updated(v, left))
                checkEqType(nEnv, v, left)
            case (pLeft: SIRType.Primitive, pRight: SIRType.Primitive) =>
                if pLeft == pRight then UnificationSuccess(env, pLeft)
                else UnificationFailure(env.path, left, right)
            case (ccLeft: SIRType.CaseClass, ccRight: SIRType.CaseClass) =>
                unifyConstrDecl(
                  ccLeft.constrDecl,
                  ccRight.constrDecl,
                  env.copy(path = "constrDecl" :: env.path)
                ) match
                    case UnificationSuccess(env1, constrDecl) =>
                        unifyList(
                          ccLeft.typeArgs,
                          ccRight.typeArgs,
                          env1.copy(path = "args" :: env.path)
                        ) match
                            case UnificationSuccess(env2, typeArgs) =>
                                ccLeft.parent match
                                    case None =>
                                        UnificationSuccess(
                                          env2.copy(path = env.path),
                                          SIRType.CaseClass(constrDecl, typeArgs, None)
                                        )
                                    case Some(p) =>
                                        unifyType(
                                          p,
                                          ccRight.parent.get,
                                          env2.copy(path = "parent" :: env2.path)
                                        ) match
                                            case UnificationSuccess(env3, parent) =>
                                                UnificationSuccess(
                                                  env3.copy(path = env.path),
                                                  SIRType.CaseClass(
                                                    constrDecl,
                                                    typeArgs,
                                                    Some(parent)
                                                  )
                                                )
                                            case failure @ UnificationFailure(path, left, right) =>
                                                failure
                            case failure @ UnificationFailure(path, left, right) => failure
                    case failure @ UnificationFailure(path, left, right) => failure
            case (ccLeft: SIRType.CaseClass, ccRight: SIRType.SumCaseClass) =>
                ccRight.decl.constructors.find(_.name == ccLeft.constrDecl.name) match
                    case Some(constrDecl) =>
                        unifyConstrDecl(
                          ccLeft.constrDecl,
                          constrDecl,
                          env.copy(path = "constrDecl" :: env.path)
                        ) match
                            case UnificationSuccess(env1, constrDecl) =>
                                // for now we have no mapping between constructor tupe argument and parent type argument,
                                // so for now just not check.
                                //  In future, insert check here, when we will have full reflection of scala type hierechy
                                UnificationSuccess(
                                  env1.copy(path = env.path),
                                  SIRType.CaseClass(constrDecl, ccLeft.typeArgs, ccLeft.parent)
                                )
                            case failure @ UnificationFailure(path, left, right) => failure
                    case None =>
                        UnificationFailure(env.path, left, right)
            case (ccLeft: SIRType.SumCaseClass, ccRight: SIRType.CaseClass) =>
                ccLeft.decl.constructors.find(_.name == ccRight.constrDecl.name) match
                    case Some(constrDecl) =>
                        unifyConstrDecl(
                          constrDecl,
                          ccRight.constrDecl,
                          env.copy(path = "constrDecl" :: env.path)
                        ) match
                            case UnificationSuccess(env1, constrDecl) =>
                                // for now we have no mapping between constructor tupe argument and parent type argument,
                                // so for now just not check.
                                //  In future, insert check here, when we will have full reflection of scala type hierechy
                                val nEnv = env1.copy(path = env.path, parentTypes = env.parentTypes)
                                UnificationSuccess(
                                  nEnv,
                                  SIRType.CaseClass(constrDecl, ccRight.typeArgs, ccRight.parent)
                                )
                            case failure @ UnificationFailure(path, left, right) => failure
                    case None =>
                        UnificationFailure(env.path, left, right)
            case (ccLeft: SIRType.SumCaseClass, ccRight: SIRType.SumCaseClass) =>
                unifyDataDecl(ccLeft.decl, ccRight.decl, env.copy(path = "decl" :: env.path)) match
                    case UnificationSuccess(env1, decl) =>
                        unifyList(
                          ccLeft.typeArgs,
                          ccRight.typeArgs,
                          env1.copy(path = "args" :: env.path)
                        ) match
                            case UnificationSuccess(env2, typeArgs) =>
                                UnificationSuccess(
                                  env2.copy(path = env.path),
                                  SIRType.SumCaseClass(decl, typeArgs)
                                )
                            case failure @ UnificationFailure(path, left, right) => failure
                    case failure @ UnificationFailure(path, left, right) => failure
            case (SIRType.Fun(inLeft, outLef), SIRType.Fun(inRight, outRight)) =>
                unifyType(inLeft, inRight, env.copy(path = "in" :: env.path)) match
                    case UnificationSuccess(env1, in) =>
                        unifyType(outLef, outRight, env1.copy(path = "out" :: env.path)) match
                            case UnificationSuccess(env2, out) =>
                                UnificationSuccess(env2.copy(path = env.path), SIRType.Fun(in, out))
                            case failure @ UnificationFailure(path, left, right) => failure
                    case failure @ UnificationFailure(path, left, right) => failure
            case (SIRType.TypeLambda(params, body), right) =>
                val nEnv = env.copy(filledTypes =
                    env.filledTypes ++ params.map(_ -> SIRType.FreeUnificator)
                )
                unifyType(body, right, nEnv)
            case (left, SIRType.TypeLambda(params, body)) =>
                val nEnv = env.copy(filledTypes =
                    env.filledTypes ++ params.map(_ -> SIRType.FreeUnificator)
                )
                unifyType(left, body, nEnv)
            case (SIRType.FreeUnificator, right) =>
                UnificationSuccess(env, right)
            case (left, SIRType.FreeUnificator) =>
                UnificationSuccess(env, left)
            case (leftProxy: SIRType.TypeProxy, right) =>
                if leftProxy.ref == null then
                    // TODO: more appropriate erro class
                    throw new RuntimeException("TypeProxy should be resolved before unification")
                else
                    env.parentTypes.get(leftProxy.ref) match
                        case Some(parentRight) =>
                            // we are in the cyclic loop.
                            if parentRight eq right then UnificationSuccess(env, right)
                            else
                                // can cause infinite loop. Mb add counter for max-level ?
                                // UnificationFailure("proxy-loop"::env.path, left, right)
                                unifyType(leftProxy.ref, right, env)
                        case None =>
                            unifyType(leftProxy.ref, right, env)
            case (left, rightProxy: SIRType.TypeProxy) =>
                if rightProxy.ref == null then
                    throw new RuntimeException("TypeProxy should be resolved before unification")
                else
                    env.parentTypes.get(rightProxy.ref) match
                        case Some(parentLeft) =>
                            if parentLeft eq left then UnificationSuccess(env, left)
                            else unifyType(left, rightProxy.ref, env)
                        case None =>
                            unifyType(left, rightProxy.ref, env)
            case (m1: SIRType.TypeNonCaseModule, m2: SIRType.TypeNonCaseModule) =>
                if m1.name == m2.name then UnificationSuccess(env, m1)
                else UnificationFailure(env.path, left, right)
            case (SIRType.TypeNothing, SIRType.TypeNothing) =>
                UnificationSuccess(env, SIRType.TypeNothing)
            case _ =>
                UnificationFailure(env.path, left, right)
    }

    given Unify[Binding] with {
        def apply(left: Binding, right: Binding, env: Env): UnificationResult[Binding] = {
            unifyBinding(left, right, env)
        }
    }

    def unifyBinding(left: Binding, right: Binding, env: Env): UnificationResult[Binding] = {
        if left.name == right.name then
            unifySIR(left.value, right.value, env.copy(path = "value" :: env.path)) match
                case UnificationSuccess(env1, value) =>
                    UnificationSuccess(env1.copy(path = env.path), Binding(left.name, value))
                case failure @ UnificationFailure(path, left, right) => failure
        else UnificationFailure(env.path, left, right)
    }

    given Unify[SIR.Decl] with {
        def apply(left: SIR.Decl, right: SIR.Decl, env: Env): UnificationResult[SIR.Decl] = {
            unifyDecl(left, right, env)
        }
    }

    def unifyDecl(left: SIR.Decl, right: SIR.Decl, env: Env): UnificationResult[SIR.Decl] = {
        unifyDataDecl(left.data, right.data, env.copy(path = "data" :: env.path)) match
            case UnificationSuccess(env1, data) =>
                unifySIR(left.term, right.term, env1.copy(path = "term" :: env.path)) match
                    case UnificationSuccess(env2, term) =>
                        UnificationSuccess(env2.copy(path = env.path), SIR.Decl(data, term))
                    case failure @ UnificationFailure(path, left, right) => failure
            case failure @ UnificationFailure(path, left, right) => failure
    }

    given Unify[TypeBinding] with {
        def apply(
            left: TypeBinding,
            right: TypeBinding,
            env: Env
        ): UnificationResult[TypeBinding] = {
            unifyTypeBinding(left, right, env)
        }
    }

    def unifyTypeBinding(
        left: TypeBinding,
        right: TypeBinding,
        env: Env
    ): UnificationResult[TypeBinding] = {
        if left.name == right.name then
            unifyType(left.tp, right.tp, env.copy(path = "tp" :: env.path)) match
                case UnificationSuccess(env1, tp) =>
                    UnificationSuccess(env1.copy(path = env.path), TypeBinding(left.name, tp))
                case failure @ UnificationFailure(path, left, right) => failure
        else UnificationFailure(env.path, left, right)
    }

    def unifyCases(
        left: List[SIR.Case],
        right: List[SIR.Case],
        env: Env
    ): UnificationResult[List[SIR.Case]] = {
        unifyList(left, right, env)
    }

    given Unify[SIR.Case] with {
        def apply(left: SIR.Case, right: SIR.Case, env: Env): UnificationResult[SIR.Case] = {
            unifyCase(left, right, env)
        }
    }

    def unifyCase(left: SIR.Case, right: SIR.Case, env: Env): UnificationResult[SIR.Case] = {
        (left.pattern, right.pattern) match
            case (
                  Pattern.Constr(leftConstr, lbindings, ltbds),
                  Pattern.Constr(rightConstr, rbindings, rtbds)
                ) =>
                unifyConstrDecl(
                  leftConstr,
                  rightConstr,
                  env.copy(path = "constrDecl" :: env.path)
                ) match
                    case UnificationSuccess(env1, constrDecl) =>
                        unifyList(
                          lbindings,
                          rbindings,
                          env1.copy(path = "bindings" :: env.path)
                        ) match
                            case UnificationSuccess(env2, bindings) =>
                                unifyList(
                                  ltbds,
                                  rtbds,
                                  env2.copy(path = "typeBindings" :: env.path)
                                ) match
                                    case UnificationSuccess(env3, typeBindings) =>
                                        unifySIR(
                                          left.body,
                                          right.body,
                                          env3.copy(path = "body" :: env.path)
                                        ) match
                                            case UnificationSuccess(env4, body) =>
                                                UnificationSuccess(
                                                  env4.copy(path = env.path),
                                                  SIR.Case(
                                                    Pattern
                                                        .Constr(constrDecl, bindings, typeBindings),
                                                    body,
                                                    left.anns
                                                  )
                                                )
                                            case failure @ UnificationFailure(path, left, right) =>
                                                failure
                                    case failure @ UnificationFailure(path, left, right) => failure
                            case failure @ UnificationFailure(path, left, right) => failure
                    case failure @ UnificationFailure(path, left, right) => failure
            case (Pattern.Wildcard, Pattern.Wildcard) =>
                unifySIR(left.body, right.body, env.copy(path = "body" :: env.path)) match
                    case UnificationSuccess(env1, body) =>
                        UnificationSuccess(env, SIR.Case(Pattern.Wildcard, body, left.anns))
                    case failure @ UnificationFailure(path, left, right) => failure
            case (_, _) =>
                UnificationFailure(env.path, left, right)
    }

    given Unify[ConstrDecl] with {
        def apply(left: ConstrDecl, right: ConstrDecl, env: Env): UnificationResult[ConstrDecl] = {
            unifyConstrDecl(left, right, env)
        }
    }

    def unifyConstrDecl(
        left: ConstrDecl,
        right: ConstrDecl,
        env: Env
    ): UnificationResult[ConstrDecl] = {
        if left eq right then UnificationSuccess(env, left)
        else if left.name == right.name && left.storageType == right.storageType then
            unifyList(left.typeParams, right.typeParams, env.copy(path = "typeParams" :: env.path))(
              using TypeVarSyntaxUnify
            ) match
                case UnificationSuccess(env1, typeParams) =>
                    unifyList(left.params, right.params, env1.copy(path = "args" :: env.path)) match
                        case UnificationSuccess(env2, params) =>
                            unifyList(
                              left.parentTypeArgs,
                              right.parentTypeArgs,
                              env2.copy(path = "parentTypeArgs" :: env.path)
                            ) match
                                case UnificationSuccess(ebv3, parentTypeArgs) =>
                                    UnificationSuccess(
                                      env2.copy(path = env.path),
                                      ConstrDecl(
                                        left.name,
                                        left.storageType,
                                        params,
                                        typeParams,
                                        parentTypeArgs,
                                        left.annotations
                                      )
                                    )
                                case failure @ UnificationFailure(path, left, right) => failure
                        case failure @ UnificationFailure(path, left, right) => failure
                case failure @ UnificationFailure(path, left, right) => failure
        else UnificationFailure(env.path, left, right)
    }

    given Unify[DataDecl] with {
        def apply(left: DataDecl, right: DataDecl, env: Env): UnificationResult[DataDecl] = {
            unifyDataDecl(left, right, env)
        }
    }

    // for now we accept to different data-decls as unrealred.
    //  things can changed in future,  when DataDecl will be able contains not only constructors
    def unifyDataDecl(left: DataDecl, right: DataDecl, env: Env): UnificationResult[DataDecl] = {
        if left eq right then UnificationSuccess(env, left)
        else if left.name == right.name then
            // order of conctructoes is determenistic (the same as in program source)
            unifyList(
              left.constructors,
              right.constructors,
              env.copy(path = "constructors" :: env.path)
            ) match
                case UnificationSuccess(env1, constructors) =>
                    unifyList(
                      left.typeParams,
                      right.typeParams,
                      env1.copy(path = "typeParams" :: env.path)
                    )(using TypeVarSyntaxUnify) match
                        case UnificationSuccess(env2, typeParams) =>
                            UnificationSuccess(
                              env2.copy(path = env.path),
                              DataDecl(left.name, constructors, typeParams, left.annotations)
                            )
                        case failure @ UnificationFailure(path, left, right) => failure
                case failure @ UnificationFailure(path, left, right) => failure
        else UnificationFailure(env.path, left, right)
    }

    def unifyList[T](left: List[T], right: List[T], env: Env)(using
        Unify[T]
    ): UnificationResult[List[T]] = {
        if left.length != right.length then UnificationFailure(env.path, left, right)
        else
            val res = left
                .zip(right)
                .foldLeft[UnificationResult[List[T]]](UnificationSuccess(env, List.empty)) {
                    case (UnificationSuccess(env, acc), (l, r)) =>
                        summon[Unify[T]].apply(l, r, env) match
                            case UnificationSuccess(env1, v) =>
                                UnificationSuccess(env1, v :: acc)
                            case failure @ UnificationFailure(path, left, right) =>
                                UnificationFailure(path, left, right)
                    case (failure @ UnificationFailure(path, left, right), _) =>
                        failure
                }
            res match
                case UnificationSuccess(env, res) =>
                    UnificationSuccess(env, res.reverse)
                case failure @ UnificationFailure(path, left, right) =>
                    failure

    }

    // should not be given,  because this contradict with using type-vars in type unification.
    //  This unification is ised when we check - are two ContrDecl or DataDecl are the same
    object TypeVarSyntaxUnify extends Unify[SIRType.TypeVar] {
        def apply(
            left: SIRType.TypeVar,
            right: SIRType.TypeVar,
            env: Env
        ): UnificationResult[SIRType.TypeVar] = {
            unifyTypeVarSyntaxically(left, right, env)
        }
    }

    def unifyTypeVarSyntaxically(
        v1: SIRType.TypeVar,
        v2: SIRType.TypeVar,
        env: Env
    ): UnificationResult[SIRType.TypeVar] = {
        if v1 == v2 then UnificationSuccess(env, v1)
        else UnificationFailure(env.path, v1, v2)
    }

}
