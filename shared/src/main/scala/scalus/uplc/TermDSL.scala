package scalus.uplc

import scalus.*
import scalus.builtin.Data
import scalus.builtin.Data.*
import scala.collection.immutable

object TermDSL:
    def applyToList(app: Term): (Term, immutable.List[Term]) =
        app match
            case Term.Apply(f, arg) =>
                val (f1, args) = applyToList(f)
                (f1, args :+ arg)
            case f => (f, Nil)

    def λ(names: String*)(term: Term): Term = lam(names: _*)(term)
    def lam(names: String*)(term: Term): Term = names.foldRight(term)(Term.LamAbs(_, _))
    def vr(name: String): Term = Term.Var(NamedDeBruijn(name))
    extension (term: Term)
        def $(rhs: Term) = Term.Apply(term, rhs)
        def unary_! = Term.Force(term)
        def unary_~ = Term.Delay(term)

    extension (sc: StringContext) def vr(args: Any*): Term = Term.Var(NamedDeBruijn(sc.parts.head))

    given Conversion[DefaultFun, Term] with
        def apply(bn: DefaultFun): Term = Term.Builtin(bn)

    given constantAsTerm[A: Constant.LiftValue]: Conversion[A, Term] with
        def apply(c: A): Term = Term.Const(summon[Constant.LiftValue[A]].lift(c))

    given Conversion[Constant, Term] with
        def apply(c: Constant): Term = Term.Const(c)

    given constantAsData[A: Data.ToData]: Conversion[A, Data] with
        def apply(c: A): Data = summon[Data.ToData[A]](c)
