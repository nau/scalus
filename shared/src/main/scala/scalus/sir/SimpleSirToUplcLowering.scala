package scalus.sir

import scalus.sir.Recursivity.*
import scalus.uplc.DefaultFun
import scalus.uplc.DefaultFun.*
import scalus.uplc.ExprBuilder
import scalus.uplc.Meaning
import scalus.uplc.NamedDeBruijn
import scalus.uplc.Term
import scalus.uplc.TermDSL.{*, given}
import scalus.uplc.TypeScheme
import scala.collection.mutable.HashMap
import scalus.uplc.Term
import scalus.uplc.Constant

class SimpleSirToUplcLowering(generateErrorTraces: Boolean = false) {

  val builtinTerms = {
    def forceBuiltin(scheme: TypeScheme, term: Term): Term = scheme match
      case TypeScheme.All(_, t) => Term.Force(forceBuiltin(t, term))
      case _                    => term

    Meaning.BuiltinMeanings.map((bi, rt) => bi -> forceBuiltin(rt.typeScheme, Term.Builtin(bi)))
  }

  private var zCombinatorNeeded: Boolean = false
  private val decls = HashMap.empty[String, DataDecl]

  def lower(sir: SIR): Term =
    val term = etaReduce(lowerInner(sir))
    if zCombinatorNeeded then Term.Apply(Term.LamAbs("__z_combinator__", term), ExprBuilder.ZTerm)
    else term

  def lowerInner(sir: SIR): Term =
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
          case None => throw new IllegalArgumentException(s"Constructor $name not found in $data")
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
      case SIR.Or(lhs, rhs) => lowerInner(SIR.IfThenElse(lhs, SIR.Const(Constant.Bool(true)), rhs))
      case SIR.Not(term) =>
        lowerInner(
          SIR.IfThenElse(term, SIR.Const(Constant.Bool(false)), SIR.Const(Constant.Bool(true)))
        )
      case SIR.IfThenElse(cond, t, f) =>
        !(builtinTerms(DefaultFun.IfThenElse) $ lowerInner(cond) $ ~lowerInner(t) $ ~lowerInner(f))
      case SIR.Builtin(bn) => builtinTerms(bn)
      case SIR.Error(msg) =>
        if generateErrorTraces
        then !(builtinTerms(DefaultFun.Trace) $ Term.Const(Constant.String(msg)) $ ~Term.Error(msg))
        else Term.Error(msg)

  def etaReduce(term: Term): Term =
    import Term.*
    term match
      case LamAbs(name1, Term.Apply(f, Term.Var(name2)))
          if name1 == name2.name && !freeNames(f, List.empty).contains(name1) && notError(f) =>
        println(
          s"etaReducing ${term.pretty.render(80).take(50)} to ${f.pretty.render(80).take(50)}"
        )
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
      case Error(_)                    => Set.empty
      case Builtin(bn)                 => Set.empty

  def freeNames(term: SIR, env: List[String]): Set[String] =
    import SIR.*
    term match
      case Var(name) =>
        if env.contains(name) then Set.empty else Set(name)
      case ExternalVar(_, name) =>
        if env.contains(name) then Set.empty else Set(name)
      case LamAbs(name, body) =>
        freeNames(body, name :: env)
      case Apply(f, arg) =>
        freeNames(f, env) ++ freeNames(arg, env)
      case Let(_, bindings, body) =>
        val env1 = bindings.map(_.name) ++ env
        bindings.foldLeft(freeNames(body, env1)) { case (acc, Binding(_, rhs)) =>
          acc ++ freeNames(rhs, env1)
        }
      case Match(scrutinee, cases) =>
        freeNames(scrutinee, env) ++ cases.foldLeft(Set.empty[String]) {
          case (acc, Case(_, bindings, body)) =>
            acc ++ freeNames(body, bindings ++ env)
        }
      case Const(_) => Set.empty
      case And(lhs, rhs) =>
        freeNames(lhs, env) ++ freeNames(rhs, env)
      case Or(lhs, rhs) =>
        freeNames(lhs, env) ++ freeNames(rhs, env)
      case Not(term) =>
        freeNames(term, env)
      case IfThenElse(cond, t, f) =>
        freeNames(cond, env) ++ freeNames(t, env) ++ freeNames(f, env)
      case Builtin(_)       => Set.empty
      case Error(_)         => Set.empty
      case Decl(data, term) => freeNames(term, env)
      case Constr(name, data, args) =>
        args.foldLeft(Set.empty[String]) { case (acc, arg) =>
          acc ++ freeNames(arg, env)
        }

  def notError(term: SIR): Boolean =
    import SIR.*
    term match
      case Error(_)                => false
      case Decl(data, term)        => notError(term)
      case Constr(_, _, args)      => args.forall(notError)
      case Apply(f, a)             => notError(f) && notError(a)
      case LamAbs(_, body)         => notError(body)
      case Let(_, bindings, body)  => bindings.forall(b => notError(b.value)) && notError(body)
      case Match(scrutinee, cases) => notError(scrutinee)
      case Const(_)                => true
      case And(lhs, rhs)           => notError(lhs) && notError(rhs)
      case Or(lhs, rhs)            => notError(lhs) && notError(rhs)
      case Not(term)               => notError(term)
      case IfThenElse(c, t, f)     => notError(c) && notError(t) && notError(f)
      case Builtin(_)              => true
      case ExternalVar(_, _)       => true
      case Var(_)                  => true

  def notError(term: Term): Boolean =
    import Term.*
    term match
      case Error(_)        => false
      case Apply(f, a)     => notError(f) && notError(a)
      case LamAbs(_, body) => notError(body)
      case Force(term)     => notError(term)
      case Delay(term)     => notError(term)
      case Const(_)        => true
      case Builtin(_)      => true
      case Var(_)          => true

  def etaReduceSIR(term: SIR): SIR =
    import SIR.*
    import scalus.pretty
    // println(s"eta: ${term.pretty.render(80).take(50)}")
    // println(s"eta: ${term.toString.take(100)}")
    term match
      // \x. f x -> f
      // \x -> \ y -> ((f x) y) => f
      case LamAbs(name1, Apply(f, Var(name2)))
          if name1 == name2 && !freeNames(f, List.empty).contains(name1) && notError(f) =>
        println(
          s"etaReducing ${term.pretty.render(80).take(50)} to ${f.pretty.render(80).take(50)}"
        )
        etaReduceSIR(f)
      case LamAbs(name, body) =>
        // println(s"lam $name: ${term.toString.take(100)}")
        val body1 = etaReduceSIR(body)
        if body != body1 then etaReduceSIR(LamAbs(name, body1)) else term
      case Apply(f, arg) => Apply(etaReduceSIR(f), etaReduceSIR(arg))
      case Let(recursivity, bindings, body) =>
        Let(
          recursivity,
          bindings.map(b => b.copy(value = etaReduceSIR(b.value))),
          etaReduceSIR(body)
        )
      case IfThenElse(cond, t, f) => IfThenElse(cond, etaReduceSIR(t), etaReduceSIR(f))
      case Match(scrutinee, cases) =>
        Match(
          scrutinee,
          cases.map { case Case(constr, bindings, body) =>
            Case(constr, bindings, etaReduceSIR(body))
          }
        )
      case Constr(name, data, args) => Constr(name, data, args.map(etaReduceSIR))
      case And(a, b)                => And(etaReduceSIR(a), etaReduceSIR(b))
      case Or(a, b)                 => Or(etaReduceSIR(a), etaReduceSIR(b))
      case Not(a)                   => Not(etaReduceSIR(a))
      case Decl(data, term)         => Decl(data, etaReduceSIR(term))
      case _                        => term
}
