package scalus.sir

import scalus.sir.Recursivity.*
import scalus.uplc.Constant
import scalus.uplc.DefaultFun
import scalus.uplc.DefaultFun.*
import scalus.uplc.ExprBuilder
import scalus.uplc.Meaning
import scalus.uplc.NamedDeBruijn
import scalus.uplc.Term
import scalus.uplc.TermDSL.*
import scalus.uplc.TypeScheme

import scala.collection.mutable.HashMap

class SimpleSirToUplcLowering(sir: SIR, generateErrorTraces: Boolean = false) {

    val builtinTerms = {
        def forceBuiltin(scheme: TypeScheme, term: Term): Term = scheme match
            case TypeScheme.All(_, t) => Term.Force(forceBuiltin(t, term))
            case _                    => term

        Meaning.plutusV2Builtins.BuiltinMeanings.map((bi, rt) =>
            bi -> forceBuiltin(rt.typeScheme, Term.Builtin(bi))
        )
    }

    private var zCombinatorNeeded: Boolean = false
    private val decls = HashMap.empty[String, DataDecl]

    def lower(): Term =
        val term = lowerInner(sir)
        if zCombinatorNeeded then
            Term.Apply(Term.LamAbs("__z_combinator__", term), ExprBuilder.ZTerm)
        else term

    private def lowerInner(sir: SIR): Term =
        sir match
            case SIR.Decl(data, body) =>
                decls(data.name) = data
                lowerInner(body)
            case SIR.Constr(name, data, args) =>
                /* data List a = Nil | Cons a (List a)
           Nil is represented as \Nil Cons -> force Nil
           Cons is represented as (\head tail Nil Cons -> Cons head tail) h tl
                 */
                val constrs = data.constructors.map(_.name)
                val ctorParams = data.constructors.find(_.name == name) match
                    case None =>
                        throw new IllegalArgumentException(s"Constructor $name not found in $data")
                    case Some(value) => value.params

                // force Nil | Cons head tail
                val appInner = ctorParams match
                    case Nil => Term.Force(Term.Var(NamedDeBruijn(name)))
                    case _ =>
                        ctorParams.foldLeft(Term.Var(NamedDeBruijn(name)))((acc, param) =>
                            acc $ Term.Var(NamedDeBruijn(param))
                        )
                // \Nil Cons -> ...
                val ctor = constrs.foldRight(appInner) { (constr, acc) =>
                    Term.LamAbs(constr, acc)
                }
                // \head tail Nil Cons -> ...
                val ctorParamsLambda = ctorParams.foldRight(ctor) { (param, acc) =>
                    Term.LamAbs(param, acc)
                }
                // (\Nil Cons -> force Nil) | (\head tail Nil Cons -> ...) h tl
                args.foldLeft(ctorParamsLambda) { (acc, arg) =>
                    Term.Apply(acc, lowerInner(arg))
                }
            case SIR.Match(scrutinee, cases) =>
                /* list match
          case Nil -> 1
          case Cons(h, tl) -> 2

          lowers to list (delay 1) (\h tl -> 2)
                 */
                val scrutineeTerm = lowerInner(scrutinee)
                val casesTerms = cases.map { case Case(constr, bindings, body) =>
                    constr.params match
                        case Nil => ~lowerInner(body)
                        case _ =>
                            bindings.foldRight(lowerInner(body)) { (binding, acc) =>
                                Term.LamAbs(binding, acc)
                            }
                }
                casesTerms.foldLeft(scrutineeTerm) { (acc, caseTerm) => Term.Apply(acc, caseTerm) }
            case SIR.Var(name)            => Term.Var(NamedDeBruijn(name))
            case SIR.ExternalVar(_, name) => Term.Var(NamedDeBruijn(name))
            case SIR.Let(NonRec, bindings, body) =>
                bindings.foldRight(lowerInner(body)) { case (Binding(name, rhs), body) =>
                    Term.Apply(Term.LamAbs(name, body), lowerInner(rhs))
                }
            case SIR.Let(Rec, Binding(name, rhs) :: Nil, body) =>
                /* let rec f x = f (x + 1)
           in f 0
           (\f -> f 0) (Z (\f. \x. f (x + 1)))
                 */
                zCombinatorNeeded = true
                val fixed =
                    Term.Apply(
                      Term.Var(NamedDeBruijn("__z_combinator__")),
                      Term.LamAbs(name, lowerInner(rhs))
                    )
                Term.Apply(Term.LamAbs(name, lowerInner(body)), fixed)
            case SIR.Let(Rec, bindings, body) =>
                sys.error(s"Mutually recursive bindings are not supported: $bindings")
            case SIR.LamAbs(name, term) => Term.LamAbs(name, lowerInner(term))
            case SIR.Apply(f, arg)      => Term.Apply(lowerInner(f), lowerInner(arg))
            case SIR.Const(const)       => Term.Const(const)
            case SIR.And(lhs, rhs) =>
                lowerInner(SIR.IfThenElse(lhs, rhs, SIR.Const(Constant.Bool(false))))
            case SIR.Or(lhs, rhs) =>
                lowerInner(SIR.IfThenElse(lhs, SIR.Const(Constant.Bool(true)), rhs))
            case SIR.Not(term) =>
                lowerInner(
                  SIR.IfThenElse(
                    term,
                    SIR.Const(Constant.Bool(false)),
                    SIR.Const(Constant.Bool(true))
                  )
                )
            /* TODO: enable this small optimization at some point
      case SIR.IfThenElse(cond, t, f) if noEval(t) && noEval(f) =>
        import scalus.pretty
        println(s"ifThenElse without force/delay: ${sir.pretty.render(80)}")
        builtinTerms(DefaultFun.IfThenElse) $ lowerInner(cond) $ lowerInner(t) $ lowerInner(f) */
            case SIR.IfThenElse(cond, t, f) =>
                !(builtinTerms(DefaultFun.IfThenElse) $ lowerInner(cond) $ ~lowerInner(
                  t
                ) $ ~lowerInner(f))
            case SIR.Builtin(bn) => builtinTerms(bn)
            case SIR.Error(msg) =>
                if generateErrorTraces
                then
                    !(builtinTerms(DefaultFun.Trace) $ Term.Const(
                      Constant.String(msg)
                    ) $ ~Term.Error)
                else Term.Error

    def noEval(term: SIR): Boolean =
        import SIR.*
        term match
            case Var(name)                        => true
            case ExternalVar(moduleName, name)    => true
            case Let(recursivity, bindings, body) => false
            case LamAbs(name, term)               => true
            case Apply(f, arg)                    => false
            case Const(const)                     => true
            case And(a, b)                        => false
            case Or(a, b)                         => false
            case Not(a)                           => false
            case IfThenElse(cond, t, f)           => false
            case Builtin(bn)                      => true
            case Error(msg)                       => false
            case Decl(data, term)                 => false
            case Constr(name, data, args)         => false
            case Match(scrutinee, cases)          => false
}

object EtaReduce {
    def etaReduce(term: Term): Term =
        import Term.*
        term match
            case LamAbs(name1, Term.Apply(f, Term.Var(name2)))
                if name1 == name2.name && !freeNames(f, List.empty).contains(name1) && notError(
                  f
                ) =>
                /* println(
          s"etaReducing ${term.pretty.render(80).take(50)} to ${f.pretty.render(80).take(50)}"
        ) */
                etaReduce(f)
            case LamAbs(name, body) =>
                val body1 = etaReduce(body)
                if body != body1 then etaReduce(LamAbs(name, body1)) else term
            case Apply(f, arg) => Apply(etaReduce(f), etaReduce(arg))
            case Force(term)   => Force(etaReduce(term))
            case Delay(term)   => Delay(etaReduce(term))
            case _             => term

    def freeNames(term: Term, env: List[String]): Set[String] =
        import Term.*
        term match
            case Var(NamedDeBruijn(name, _)) => if env.contains(name) then Set.empty else Set(name)
            case LamAbs(name, body)          => freeNames(body, name :: env)
            case Apply(f, arg)               => freeNames(f, env) ++ freeNames(arg, env)
            case Force(term)                 => freeNames(term, env)
            case Delay(term)                 => freeNames(term, env)
            case Const(_)                    => Set.empty
            case Error                       => Set.empty
            case Builtin(bn)                 => Set.empty

    def notError(term: Term): Boolean =
        import Term.*
        term match
            case Error           => false
            case Apply(f, a)     => notError(f) && notError(a)
            case LamAbs(_, body) => notError(body)
            case Force(term)     => notError(term)
            case Delay(term)     => notError(term)
            case Const(_)        => true
            case Builtin(_)      => true
            case Var(_)          => true
}
