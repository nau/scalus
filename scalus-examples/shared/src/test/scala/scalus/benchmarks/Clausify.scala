package scalus.benchmarks

import scala.language.implicitConversions
import scala.annotation.nowarn
import scalus.*
import scalus.prelude.{*, given}
import scalus.prelude.Eq.{*, given}
import scalus.prelude.Ord.{*, given}
import org.scalatest.funsuite.AnyFunSuite
import scalus.testkit.ScalusTest

class Clausify extends AnyFunSuite, ScalusTest:
    import Clausify.{*, given}
    import Syms.*

    test("F0") {
        val result = Compiler
            .compile {
                val formula = (_1 <-> _1) <-> ((_1 <-> _1) <-> (_1 <-> _1))
                formula.clauses === List.empty[LRVars]
            }
            .scriptV3()
            .evaluateDebug
//        println("--------------- " + result)
    }

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
        val expected = List[LRVars]((List.single(1), List.empty[Var]))
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

@Compile
object Clausify:
    type Var = BigInt
    type LRVars = (List[Var], List[Var])

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

    @nowarn @Ignore
    inline given Conversion[Var, Formula] = (arg: Var) => arg.toFormula
    @nowarn @Ignore
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
                case Sym(arg)                => self
                case Not(arg)                => !arg.eliminate
                case And(arg1, arg2)         => arg1.eliminate && arg2.eliminate
                case Or(arg1, arg2)          => arg1.eliminate || arg2.eliminate
                case Implication(arg1, arg2) => !arg1.eliminate || arg2.eliminate
                case Equivalence(arg1, arg2) => (arg1 -> arg2).eliminate && (arg2 -> arg1).eliminate

        /// -- shift negation to innermost positions
        def negin: Formula =
            self match
                case Not(expr) =>
                    expr match
                        case Not(arg)        => arg.negin
                        case And(arg1, arg2) => (!arg1).negin || (!arg2).negin
                        case Or(arg1, arg2)  => (!arg1).negin && (!arg2).negin
                        case _               => self
                case And(arg1, arg2) => arg1.negin && arg2.negin
                case Or(arg1, arg2)  => arg1.negin || arg2.negin
                case _               => self

        /// shift disjunction within conjunction
        def disin: Formula = {
            extension (self: Formula)
                def isAnd: Boolean = self match { case And(_, _) => true; case _ => false }

                def unwrapAnd: (Formula, Formula) =
                    self match
                        case And(lhs, rhs) => (lhs, rhs)
                        case _             => throw IllegalArgumentException("Not an And formula")

            end extension

            self match
                case Or(left, right) =>
                    if right.isAnd then
                        val (rightArg1, rightArg2) = right.unwrapAnd
                        (left || rightArg1).disin && (left || rightArg2).disin
                    else if left.isAnd then
                        val (leftArg1, leftArg2) = left.unwrapAnd
                        (leftArg1 || right).disin && (leftArg2 || right).disin
                    else
                        val leftDisin = left.disin
                        val rightDisin = right.disin
                        if leftDisin.isAnd || rightDisin.isAnd then (leftDisin || rightDisin).disin
                        else leftDisin || rightDisin
                case And(arg1, arg2) => arg1.disin && arg2.disin
                case _               => self
        }

        /// split conjunctive proposition into a list of conjuncts
        def split: List[Formula] = {
            def doSplit(formula: Formula, list: List[Formula]): List[Formula] =
                formula match
                    case And(arg1, arg2) => doSplit(arg1, doSplit(arg2, list))
                    case _               => list.prepended(formula)

            doSplit(self, List.empty)
        }

        def clauses: List[LRVars] = eliminate.negin.disin.split.unicl

    end extension

    extension (self: List[Formula])
        /// form unique clausal axioms excluding tautologies
        def unicl: List[LRVars] = {
            extension [A: Ord](self: List[A])
                def insertUniqueOrdered(elem: A): List[A] =
                    self match
                        case List.Nil => List.single(elem)
                        case List.Cons(head, tail) =>
                            elem <=> head match
                                case Order.Less    => self.prepended(elem)
                                case Order.Greater => tail.insertUniqueOrdered(elem).prepended(head)
                                case Order.Equal   => self

            end extension

            extension (self: Formula)
                def clause: LRVars =
                    def doClause(formula: Formula, vars: LRVars): LRVars =
                        formula match
                            case Or(arg1, arg2) => doClause(arg1, doClause(arg2, vars))
                            case Sym(arg)       => (vars._1.insertUniqueOrdered(arg), vars._2)
                            case Not(expr) =>
                                expr match
                                    case Sym(arg) =>
                                        (vars._1, vars._2.insertUniqueOrdered(arg))
                                    case _ => fail("Invalid formula")
                            case _ => fail("Invalid formula")

                    doClause(self, (List.empty, List.empty))

            end extension

            self.foldRight(List.empty) { (formula, list) =>
                val lrVars = formula.clause
                if lrVars._1.exists(lrVars._2.contains(_)) then list
                else list.insertUniqueOrdered(lrVars)
            }
        }

    end extension

end Clausify

@Compile
object Syms:
    import Clausify.Formula.Sym

    val _1 = Sym(BigInt(1))
    val _2 = Sym(BigInt(2))
    val _3 = Sym(BigInt(3))
    val _4 = Sym(BigInt(4))
    val _5 = Sym(BigInt(5))
    val _6 = Sym(BigInt(6))
    val _7 = Sym(BigInt(7))

end Syms
