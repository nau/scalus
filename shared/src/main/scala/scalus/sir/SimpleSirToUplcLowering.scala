package scalus.sir

import scalus.sir.Recursivity.*
import scalus.uplc.DefaultFun.*
import scalus.uplc.TermDSL.*
import scalus.uplc.{DefaultFun, ExprBuilder, Meaning, Term, TypeScheme}

class SimpleSirToUplcLowering {

  val builtinTerms = {
    def forceBuiltin(scheme: TypeScheme, term: Term): Term = scheme match
      case TypeScheme.All(_, t) => Term.Force(forceBuiltin(t, term))
      case _                    => term

    Meaning.BuiltinMeanings.map((bi, rt) => bi -> forceBuiltin(rt.typeScheme, Term.Builtin(bi)))
  }

  def lower(sir: SIR): Term =
    sir match
      case SIR.Var(name) => Term.Var(name)
      case SIR.Let(NonRec, bindings, body) =>
        bindings.foldRight(lower(body)) { case (Binding(name, rhs), body) =>
          Term.Apply(Term.LamAbs(name, body), lower(rhs))
        }
      case SIR.Let(Rec, Binding(name, rhs) :: Nil, body) =>
        /* let rec f x = f (x + 1)
           in f 0
           (\f -> f 0) (Z (\f. \x. f (x + 1)))
         */
        val fixed = Term.Apply(ExprBuilder.ZTerm, Term.LamAbs(name, lower(rhs)))
        Term.Apply(Term.LamAbs(name, lower(body)), fixed)
      case SIR.Let(Rec, bindings, body) =>
        sys.error(s"Mutually recursive bindings are not supported: $bindings")
      case SIR.LamAbs(name, term) => Term.LamAbs(name, lower(term))
      case SIR.Apply(f, arg)      => Term.Apply(lower(f), lower(arg))
      case SIR.Const(const)       => Term.Const(const)
      case SIR.IfThenElse(cond, t, f) =>
        !(builtinTerms(DefaultFun.IfThenElse) $ lower(cond) $ ~lower(t) $ ~lower(f))
      case SIR.Builtin(bn) => builtinTerms(bn)
      case SIR.Error(msg)  => Term.Error(msg)

}
