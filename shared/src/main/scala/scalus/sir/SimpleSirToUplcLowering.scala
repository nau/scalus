package scalus.sir

import scalus.sir.Recursivity.*
import scalus.uplc.DefaultFun
import scalus.uplc.DefaultFun.*
import scalus.uplc.ExprBuilder
import scalus.uplc.Meaning
import scalus.uplc.NamedDeBruijn
import scalus.uplc.Term
import scalus.uplc.TermDSL.*
import scalus.uplc.TypeScheme

class SimpleSirToUplcLowering {

  val builtinTerms = {
    def forceBuiltin(scheme: TypeScheme, term: Term): Term = scheme match
      case TypeScheme.All(_, t) => Term.Force(forceBuiltin(t, term))
      case _                    => term

    Meaning.BuiltinMeanings.map((bi, rt) => bi -> forceBuiltin(rt.typeScheme, Term.Builtin(bi)))
  }

  private var zCombinatorNeeded: Boolean = false

  def lower(sir: SIR): Term =
    val term = lowerInner(sir)
    if zCombinatorNeeded then Term.Apply(Term.LamAbs("__z_combinator__", term), ExprBuilder.ZTerm)
    else term

  def lowerInner(sir: SIR): Term =
    sir match
      case SIR.Var(name) => Term.Var(name)
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
      case SIR.IfThenElse(cond, t, f) =>
        !(builtinTerms(DefaultFun.IfThenElse) $ lowerInner(cond) $ ~lowerInner(t) $ ~lowerInner(f))
      case SIR.Builtin(bn) => builtinTerms(bn)
      case SIR.Error(msg)  => Term.Error(msg)
}
