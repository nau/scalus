package scalus.benchmarks

import scala.language.implicitConversions
import scala.annotation.nowarn
import scalus.*
import scalus.prelude.{*, given}
import org.scalatest.funsuite.AnyFunSuite

class Clausify extends AnyFunSuite:
    import Clausify.{*, given}

    test("F1") {
        // (a = a) = (a = a) = (a = a)
        val formula = (1 <-> 1) <-> ((1 <-> 1) <-> (1 <-> 1))
        val expected = List.empty[LRVars]
        FormulaTestCase(formula, expected).check()
    }

    test("F2") {
        // (a = a = a) = (a = a = a)
        val formula = (1 <-> (1 <-> 1)) <-> (1 <-> (1 <-> 1))
        val expected = List.empty[LRVars]
        FormulaTestCase(formula, expected).check()
    }

    test("F3") {
        // (a = a = a) = (a = a) = (a = a)
        val formula = (1 <-> (1 <-> 1)) <-> ((1 <-> 1) <-> (1 <-> 1))
        val expected = List[LRVars]((List.single(1), List.empty))
        FormulaTestCase(formula, expected).check()
    }

    test("F4") {
        // (a = b = c) = (d = e) = (f = g)
        val formula = (1 <-> (2 <-> 3)) <-> ((4 <-> 5) <-> (6 <-> 7))
        val expected = List[LRVars](
          (List(1), List(2, 3, 4, 5, 6, 7)),
          (List(1, 2, 3), List(4, 5, 6, 7)),
          (List(1, 2, 3, 4, 5), List(6, 7)),
          (List(1, 2, 3, 4, 5, 6, 7), List()),
          (List(1, 2, 3, 4, 6), List(5, 7)),
          (List(1, 2, 3, 4, 7), List(5, 6)),
          (List(1, 2, 3, 5, 6), List(4, 7)),
          (List(1, 2, 3, 5, 7), List(4, 6)),
          (List(1, 2, 3, 6, 7), List(4, 5)),
          (List(1, 2, 4), List(3, 5, 6, 7)),
          (List(1, 2, 4, 5, 6), List(3, 7)),
          (List(1, 2, 4, 5, 7), List(3, 6)),
          (List(1, 2, 4, 6, 7), List(3, 5)),
          (List(1, 2, 5), List(3, 4, 6, 7)),
          (List(1, 2, 5, 6, 7), List(3, 4)),
          (List(1, 2, 6), List(3, 4, 5, 7)),
          (List(1, 2, 7), List(3, 4, 5, 6)),
          (List(1, 3, 4), List(2, 5, 6, 7)),
          (List(1, 3, 4, 5, 6), List(2, 7)),
          (List(1, 3, 4, 5, 7), List(2, 6)),
          (List(1, 3, 4, 6, 7), List(2, 5)),
          (List(1, 3, 5), List(2, 4, 6, 7)),
          (List(1, 3, 5, 6, 7), List(2, 4)),
          (List(1, 3, 6), List(2, 4, 5, 7)),
          (List(1, 3, 7), List(2, 4, 5, 6)),
          (List(1, 4, 5), List(2, 3, 6, 7)),
          (List(1, 4, 5, 6, 7), List(2, 3)),
          (List(1, 4, 6), List(2, 3, 5, 7)),
          (List(1, 4, 7), List(2, 3, 5, 6)),
          (List(1, 5, 6), List(2, 3, 4, 7)),
          (List(1, 5, 7), List(2, 3, 4, 6)),
          (List(1, 6, 7), List(2, 3, 4, 5)),
          (List(2), List(1, 3, 4, 5, 6, 7)),
          (List(2, 3, 4), List(1, 5, 6, 7)),
          (List(2, 3, 4, 5, 6), List(1, 7)),
          (List(2, 3, 4, 5, 7), List(1, 6)),
          (List(2, 3, 4, 6, 7), List(1, 5)),
          (List(2, 3, 5), List(1, 4, 6, 7)),
          (List(2, 3, 5, 6, 7), List(1, 4)),
          (List(2, 3, 6), List(1, 4, 5, 7)),
          (List(2, 3, 7), List(1, 4, 5, 6)),
          (List(2, 4, 5), List(1, 3, 6, 7)),
          (List(2, 4, 5, 6, 7), List(1, 3)),
          (List(2, 4, 6), List(1, 3, 5, 7)),
          (List(2, 4, 7), List(1, 3, 5, 6)),
          (List(2, 5, 6), List(1, 3, 4, 7)),
          (List(2, 5, 7), List(1, 3, 4, 6)),
          (List(2, 6, 7), List(1, 3, 4, 5)),
          (List(3), List(1, 2, 4, 5, 6, 7)),
          (List(3, 4, 5), List(1, 2, 6, 7)),
          (List(3, 4, 5, 6, 7), List(1, 2)),
          (List(3, 4, 6), List(1, 2, 5, 7)),
          (List(3, 4, 7), List(1, 2, 5, 6)),
          (List(3, 5, 6), List(1, 2, 4, 7)),
          (List(3, 5, 7), List(1, 2, 4, 6)),
          (List(3, 6, 7), List(1, 2, 4, 5)),
          (List(4), List(1, 2, 3, 5, 6, 7)),
          (List(4, 5, 6), List(1, 2, 3, 7)),
          (List(4, 5, 7), List(1, 2, 3, 6)),
          (List(4, 6, 7), List(1, 2, 3, 5)),
          (List(5), List(1, 2, 3, 4, 6, 7)),
          (List(5, 6, 7), List(1, 2, 3, 4)),
          (List(6), List(1, 2, 3, 4, 5, 7)),
          (List(7), List(1, 2, 3, 4, 5, 6))
        )
        FormulaTestCase(formula, expected).check()
    }

    test("F5") {
        // (a = a = a) = (a = a = a) = (a = a)
        val formula = (1 <-> (1 <-> 1)) <-> ((1 <-> (1 <-> 1)) <-> (1 <-> 1))
        val expected = List.empty[LRVars]
        FormulaTestCase(formula, expected).check()
    }

    case class FormulaTestCase(formula: Formula, expected: List[LRVars]):
        def check(): Unit = assert(formula.clauses === expected)

end Clausify

//@Compile
object Clausify:
    type Var = BigInt
    type LRVars = (List[Var], List[Var])

    given Eq[LRVars] = (lhs: LRVars, rhs: LRVars) => lhs(0) === rhs(0) && lhs(1) === rhs(1)

    enum Formula:
        case Sym(arg: Var)
        case Not(arg: Formula)
        case And(arg1: Formula, arg2: Formula)
        case Or(arg1: Formula, arg2: Formula)
        case Implication(arg1: Formula, arg2: Formula)
        case Equivalence(arg1: Formula, arg2: Formula)

    import Formula.*

    extension (self: Var) inline def toFormula: Formula = Sym(self)
    extension (self: Int) @Ignore inline def toFormula: Formula = Sym(self)

    @nowarn
    inline given Conversion[Var, Formula] = (arg: Var) => arg.toFormula

    @nowarn
    @Ignore
    inline given Conversion[Int, Formula] = (arg: Int) => arg.toFormula

    extension (self: Formula)
        inline def unary_! : Formula = Not(self)
        inline def not: Formula = !self
        inline infix def &&(other: Formula): Formula = And(self, other)
        inline infix def and(other: Formula): Formula = self && other
        inline infix def ||(other: Formula): Formula = Or(self, other)
        inline infix def or(other: Formula): Formula = self || other
        inline infix def ->(other: Formula): Formula = Implication(self, other)
        inline infix def impl(other: Formula): Formula = self -> other
        inline infix def <->(other: Formula): Formula = Equivalence(self, other)
        inline infix def eqv(other: Formula): Formula = self <-> other

        /// eliminate connectives other than not, disjunction and conjunction
        def eliminate: Formula =
            self match
                case sym @ Sym(arg)          => sym
                case Not(arg)                => !arg.eliminate
                case And(arg1, arg2)         => arg1.eliminate && arg2.eliminate
                case Or(arg1, arg2)          => arg1.eliminate || arg2.eliminate
                case Implication(arg1, arg2) => !arg1.eliminate || arg2.eliminate
                case Equivalence(arg1, arg2) => (arg1 -> arg2).eliminate && (arg2 -> arg1).eliminate

        /// -- shift negation to innermost positions
        def negin: Formula =
            self match
                case Not(Not(arg))        => arg.negin
                case Not(And(arg1, arg2)) => (!arg1).negin || (!arg2).negin
                case Not(Or(arg1, arg2))  => (!arg1).negin && (!arg2).negin
                case And(arg1, arg2)      => arg1.negin && arg2.negin
                case Or(arg1, arg2)       => arg1.negin || arg2.negin
                case other                => other

        /// shift disjunction within conjunction
        def disin: Formula =
            self match
                case Or(arg1, And(arg2, arg3)) => (arg1 || arg2).disin && (arg1 || arg3).disin
                case Or(And(arg1, arg2), arg3) => (arg1 || arg3).disin && (arg2 || arg3).disin
                case Or(arg1, arg2) =>
                    val disinArg1 = arg1.disin
                    val disinArg2 = arg2.disin
                    if disinArg1.isInstanceOf[Formula.And] || disinArg2.isInstanceOf[Formula.And]
                    then (disinArg1 || disinArg2).disin
                    else disinArg1 || disinArg2
                case And(arg1, arg2) => arg1.disin && arg2.disin
                case other           => other

        /// split conjunctive proposition into a list of conjuncts
        def split: List[Formula] =
            def doSplit(formula: Formula, list: List[Formula]): List[Formula] =
                formula match
                    case And(arg1, arg2) => doSplit(arg1, doSplit(arg2, list))
                    case other           => list.prepended(other)

            doSplit(self, List.empty)

        def clauses: List[LRVars] = self.eliminate.negin.disin.split.unicl

    end extension

    extension (self: List[Formula])
        /// form unique clausal axioms excluding tautologies
        def unicl: List[LRVars] =
            given Ord[LRVars] = (lhs, rhs) =>
                val comp = lhs(0) <=> rhs(0)
                if comp !== BigInt(0) then comp else lhs(1) <=> rhs(1)

            extension [A: Ord](self: List[A])
                def insertUniqueOrdered(elem: A): List[A] =
                    self match
                        case List.Nil => List.single(elem)
                        case List.Cons(head, tail) =>
                            val comp = elem <=> head
                            if comp < 0 then self.prepended(elem)
                            else if comp > 0 then tail.insertUniqueOrdered(elem).prepended(head)
                            else self

            end extension

            extension (self: Formula)
                def clause: LRVars =
                    def doClause(formula: Formula, vars: LRVars): LRVars =
                        formula match
                            case Or(arg1, arg2) => doClause(arg1, doClause(arg2, vars))
                            case Sym(arg)       => (vars(0).insertUniqueOrdered(arg), vars(1))
                            case Not(Sym(arg))  => (vars(0), vars(1).insertUniqueOrdered(arg))
                            case _              => fail("Invalid formula")

                    doClause(self, (List.empty, List.empty))

            end extension

            self.foldRight(List.empty) { (formula, list) =>
                val lrVars = formula.clause
                if lrVars(0).exists(lrVars(1).contains(_)) then list
                else list.insertUniqueOrdered(lrVars)
            }

    end extension

end Clausify
