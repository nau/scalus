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

    /** Applies the argument to the term. */
    infix def $(rhs: Term): Term = Term.Apply(this, rhs)

    /** Forces the term. */
    def unary_! : Term = Term.Force(this)

    /** Delays the term. */
    def unary_~ : Term = Term.Delay(this)

    def applyToList: (Term, immutable.List[Term]) =
        this match
            case Term.Apply(f, arg) =>
                val (f1, args) = f.applyToList
                (f1, args :+ arg)
            case f => (f, Nil)

    /** Converts a term with named variables to a term with De Bruijn indices. We use unique
      * negative indices to represent free variables.
      * @return
      *   the term with De Bruijn indices
      */
    def deBruijned: Term = DeBruijn.deBruijnTerm(this)

    /** Determines alpha-equivalence between two UPLC terms.
      *
      * Alpha-equivalence considers two lambda terms equal if they have the same structure and
      * differ only in the names of bound variables. This function checks if the terms are
      * structurally identical with matching variable indices.
      *
      * @param t2
      *   The second term to compare
      * @return
      *   True if the terms are alpha-equivalent, false otherwise
      * @note
      *   This function expects the term to be de Bruijn indexed. Use [[DeBruijn.deBruijnTerm()]]
      *   before calling this function.
      * @example
      *   {{{
      *   // These terms are alpha-equivalent (same structure, different variable names)
      *   val term1 = lam("x")(x => x).deBruijned
      *   val term2 = lam("y")(y => y).deBruijned
      *   val result = term1 alphaEq term2 // Returns true
      *
      *   // These terms are not equivalent (different structure)
      *   val term3 = lam("x")(x => !x).deBruijned
      *   val term4 = lam("x")(x => x).deBruijned
      *   val result2 = term3.alphaEq(term4) // Returns false
      *   }}}
      */
    infix def alphaEq(other: Term): Boolean = Term.alphaEq(this, other)

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
    /** Determines alpha-equivalence between two UPLC terms.
      *
      * Alpha-equivalence considers two lambda terms equal if they have the same structure and
      * differ only in the names of bound variables. This function checks if the terms are
      * structurally identical with matching variable indices.
      *
      * @param t1
      *   The first term to compare
      * @param t2
      *   The second term to compare
      * @return
      *   True if the terms are alpha-equivalent, false otherwise
      * @note
      *   This function expects the term to be de Bruijn indexed. Use [[DeBruijn.deBruijnTerm()]]
      *   before calling this function.
      * @example
      *   {{{
      *   // These terms are alpha-equivalent (same structure, different variable names)
      *   val term1 = lam("x")(x => x).deBruijned
      *   val term2 = lam("y")(y => y).deBruijned
      *   val result = alphaEq(term1, term2) // Returns true
      *
      *   // These terms are not equivalent (different structure)
      *   val term3 = lam("x")(x => !x).deBruijned
      *   val term4 = lam("x")(x => x).deBruijned
      *   val result2 = alphaEq(term3, term4) // Returns false
      *   }}}
      */
    def alphaEq(t1: Term, t2: Term): Boolean =

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

    extension (sc: StringContext)
        /** Creates a variable term.
          * @param args
          *   the arguments
          * @example
          *   {{{
          *   val idx = 0
          *   vr"foo${idx}" == Var(NamedDeBruijn("foo0"))
          *   }}}
          */
        def vr(args: Any*): Term = Term.Var(NamedDeBruijn(sc.parts.head))

    extension [A: Constant.LiftValue](a: A)
        def asTerm: Term = Term.Const(summon[Constant.LiftValue[A]].lift(a))

    /** Creates a lambda abstraction with a single parameter.
      *
      * This is a helper method that simplifies creating lambda terms in the UPLC AST. It
      * automatically creates a variable reference with the provided name and passes it to the
      * function body constructor.
      *
      * @param name
      *   The name to use for the bound variable
      * @param f
      *   A function that constructs the body of the lambda using the bound variable
      * @return
      *   A new lambda abstraction term
      *
      * @example
      *   {{{
      *   val identity = lam("x")(x => x) // Creates a lambda term equivalent to: λx. x
      *   val forceTerm = lam("x")(x => !x) // Creates a lambda term equivalent to: λx. (force x)
      *   }}}
      */
    def lam(name: String)(f: Term => Term): Term = LamAbs(name, f(vr(name)))

    /** Creates a lambda abstraction with two parameters.
      *
      * This is a helper method that simplifies creating nested lambda terms in the UPLC AST. It
      * automatically creates variable references with the provided names and passes them to the
      * function body constructor.
      *
      * @param a
      *   The name to use for the first bound variable
      * @param b
      *   The name to use for the second bound variable
      * @param f
      *   A function that constructs the body of the lambda using the bound variables
      * @return
      *   A new lambda abstraction term with nested structure
      *
      * @example
      *   {{{
      *   // Creates a lambda term equivalent to: λa. λb. a
      *   val first = lam("a", "b")((a, b) => a)
      *
      *   // Creates a lambda term equivalent to: λx. λy. (force [x y])
      *   val applyForce = lam("x", "y")((x, y) => !(x $ y))
      *   }}}
      */
    def lam(a: String, b: String)(f: (Term, Term) => Term): Term =
        LamAbs(a, LamAbs(b, f(vr(a), vr(b))))

    /** Creates a lambda abstraction with three parameters.
      *
      * This is a helper method that simplifies creating nested lambda terms in the UPLC AST. It
      * automatically creates variable references with the provided names and passes them to the
      * function body constructor.
      *
      * @param a
      *   The name to use for the first bound variable
      * @param b
      *   The name to use for the second bound variable
      * @param c
      *   The name to use for the third bound variable
      * @param f
      *   A function that constructs the body of the lambda using the bound variables
      * @return
      *   A new lambda abstraction term with nested structure
      *
      * @example
      *   {{{
      *   // Creates a lambda term equivalent to: λa. λb. λc. a
      *   val first = lam("a", "b", "c")((a, b, c) => a)
      *
      *   // Creates a lambda term equivalent to: λx. λy. λz. (force [x y z])
      *   val applyForce = lam("x", "y", "z")((x, y, z) => !(x $ y $ z))
      *   }}}
      */
    def lam(a: String, b: String, c: String)(f: (Term, Term, Term) => Term): Term =
        LamAbs(a, LamAbs(b, LamAbs(c, f(vr(a), vr(b), vr(c)))))

    /** Creates a lambda abstraction with a single parameter.
      *
      * This is a helper method that simplifies creating lambda terms in the UPLC AST. It
      * automatically creates a variable reference with the provided name and passes it to the
      * function body constructor.
      *
      * @param name
      *   The name to use for the bound variable
      * @param f
      *   A function that constructs the body of the lambda using the bound variable
      * @return
      *   A new lambda abstraction term
      *
      * @example
      *   {{{
      *   val identity = λ("x")(x => x) // Creates a lambda term equivalent to: λx. x
      *   val forceTerm = λ("x")(x => !x) // Creates a lambda term equivalent to: λx. (force x)
      *   }}}
      */

    inline def λ(name: String)(f: Term => Term): Term = lam(name)(f)

    /** Creates a lambda abstraction with two parameters.
      *
      * This is a helper method that simplifies creating nested lambda terms in the UPLC AST. It
      * automatically creates variable references with the provided names and passes them to the
      * function body constructor.
      *
      * @param a
      *   The name to use for the first bound variable
      * @param b
      *   The name to use for the second bound variable
      * @param f
      *   A function that constructs the body of the lambda using the bound variables
      * @return
      *   A new lambda abstraction term with nested structure
      *
      * @example
      *   {{{
      *   // Creates a lambda term equivalent to: λa. λb. a
      *   val first = λ("a", "b")((a, b) => a)
      *
      *   // Creates a lambda term equivalent to: λx. λy. (force [x y])
      *   val applyForce = λ("x", "y")((x, y) => !(x $ y))
      *   }}}
      */
    inline def λ(a: String, b: String)(f: (Term, Term) => Term): Term = lam(a, b)(f)

    /** Creates a lambda abstraction with three parameters.
      *
      * This is a helper method that simplifies creating nested lambda terms in the UPLC AST. It
      * automatically creates variable references with the provided names and passes them to the
      * function body constructor.
      *
      * @param a
      *   The name to use for the first bound variable
      * @param b
      *   The name to use for the second bound variable
      * @param c
      *   The name to use for the third bound variable
      * @param f
      *   A function that constructs the body of the lambda using the bound variables
      * @return
      *   A new lambda abstraction term with nested structure
      *
      * @example
      *   {{{
      *   // Creates a lambda term equivalent to: λa. λb. λc. a
      *   val first = λ("a", "b", "c")((a, b, c) => a)
      *
      *   // Creates a lambda term equivalent to: λx. λy. λz. (force [x y z])
      *   val applyForce = λ("x", "y", "z")((x, y, z) => !(x $ y $ z))
      *   }}}
      */
    inline def λ(a: String, b: String, c: String)(f: (Term, Term, Term) => Term): Term =
        lam(a, b, c)(f)

    /** Creates a variable term with the given name.
      *
      * This is a helper method that simplifies creating variable references in the UPLC AST. It
      * automatically creates a [[NamedDeBruijn]] with the provided name and default index of 0.
      *
      * @param name
      *   The name to use for the variable
      * @return
      *   A new variable term
      *
      * @example
      *   {{{
      *   // Creates a variable term referencing "x"
      *   val xVar = vr("x")
      *
      *   // Can be used directly in term construction
      *   val application = Apply(vr("f"), vr("arg"))
      *   }}}
      */
    def vr(name: String): Term = Term.Var(NamedDeBruijn(name))
