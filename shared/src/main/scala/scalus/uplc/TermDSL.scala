package scalus.uplc

import scalus.*
import scalus.builtin.Data
import scala.collection.immutable

object TermDSL:
    @deprecated("Use Term.applyToList instead", "0.9.0")
    def applyToList(app: Term): (Term, immutable.List[Term]) =
        app match
            case Term.Apply(f, arg) =>
                val (f1, args) = applyToList(f)
                (f1, args :+ arg)
            case f => (f, Nil)

    @deprecated("Use Term.λ instead", "0.9.0")
    def λ(names: String*)(term: Term): Term = lam(names*)(term)
    @deprecated("Use Term.λλ instead", "0.9.0")
    def λλ(name: String)(f: Term => Term): Term = lam(name)(f(vr(name)))
    @deprecated("Use Term.lam instead", "0.9.0")
    def lam(names: String*)(term: Term): Term = names.foldRight(term)(Term.LamAbs(_, _))
    @deprecated("Use Term.vr instead", "0.9.0")
    def vr(name: String): Term = Term.Var(NamedDeBruijn(name))

    extension (term: Term)
        @deprecated("Use Term.$ instead", "0.9.0")
        infix def $(rhs: Term): Term = Term.Apply(term, rhs)
        @deprecated("Use Term.asTerm instead", "0.9.0")
        def unary_! : Term = Term.Force(term)
        @deprecated("Use Term.asTerm instead", "0.9.0")
        def unary_~ : Term = Term.Delay(term)

    extension (sc: StringContext)
        @deprecated("Use Term.vr instead", "0.9.0")
        def vr(args: Any*): Term = Term.Var(NamedDeBruijn(sc.parts.head))

    given Conversion[DefaultFun, Term] with
        def apply(bn: DefaultFun): Term = Term.Builtin(bn)

    given constantAsTerm[A: Constant.LiftValue]: Conversion[A, Term] with
        def apply(c: A): Term = Term.Const(summon[Constant.LiftValue[A]].lift(c))

    extension [A: Constant.LiftValue](a: A)
        @deprecated("Use Term.asTerm instead", "0.9.0")
        def asTerm: Term = Term.Const(summon[Constant.LiftValue[A]].lift(a))

    given Conversion[Constant, Term] with
        def apply(c: Constant): Term = Term.Const(c)

    given constantAsData[A: Data.ToData]: Conversion[A, Data] with
        def apply(c: A): Data = summon[Data.ToData[A]](c)
