package scalus.uplc

import scalus.*
import scalus.builtin.Data.*

import scala.collection.immutable

case class NamedDeBruijn(name: String, index: Int = 0):
    override def toString: String =
        if index == 0 then s"NamedDeBruijn(\"$name\")"
        else s"NamedDeBruijn(\"$name\", $index)"

enum Term:
    case Var(name: NamedDeBruijn) extends Term
    case LamAbs(name: String, term: Term) extends Term
    case Apply(f: Term, arg: Term) extends Term
    case Force(term: Term) extends Term
    case Delay(term: Term) extends Term
    case Const(const: Constant) extends Term
    case Builtin(bn: DefaultFun) extends Term
    case Error extends Term
    case Constr(tag: Long, args: immutable.List[Term])
    case Case(arg: Term, cases: immutable.List[Term])

    override def toString: String = this match
        case Var(name)          => s"Var(NamedDeBruijn(\"${name.name}\"))"
        case LamAbs(name, term) => s"LamAbs(\"$name\", $term)"
        case Apply(f, arg)      => s"Apply($f, $arg)"
        case Force(term)        => s"Force($term)"
        case Delay(term)        => s"Delay($term)"
        case Const(const)       => s"Const($const)"
        case Builtin(bn)        => s"Builtin($bn)"
        case Error              => "Error"
        case Constr(tag, args)  => s"Constr($tag, ${args.mkString(", ")})"
        case Case(arg, cases)   => s"Case($arg, ${cases.mkString(", ")})"

object Term:
    def alphaEq(t1: Term, t2: Term): Boolean =
        import Term.*

        def eqName(n1: NamedDeBruijn, n2: NamedDeBruijn): Boolean =
            assert(n1.index != 0)
            assert(n2.index != 0)
            n1.index == n2.index

        def equals(self: Term, other: Term): Boolean = (self, other) match
            case (Var(n1), Var(n2))               => eqName(n1, n2)
            case (LamAbs(n1, t1), LamAbs(n2, t2)) => equals(t1, t2)
            case (Apply(f1, a1), Apply(f2, a2))   => equals(f1, f2) && equals(a1, a2)
            case (Force(t1), Force(t2))           => equals(t1, t2)
            case (Delay(t1), Delay(t2))           => equals(t1, t2)
            case (Const(c1), Const(c2))           => c1 == c2
            case (Builtin(b1), Builtin(b2))       => b1 == b2
            case (Error, Error)                   => true
            case (Constr(tag1, args1), Constr(tag2, args2)) =>
                tag1 == tag2 && args1.size == args2.size && args1
                    .zip(args2)
                    .forall((t1, t2) => equals(t1, t2))
            case (Case(arg1, cases1), Case(arg2, cases2)) =>
                arg1 == arg2 && cases1.size == cases2.size && cases1
                    .zip(cases2)
                    .forall((t1, t2) => equals(t1, t2))
            case _ => false

        equals(t1, t2)
