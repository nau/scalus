package scalus.bugtracking

import scala.language.implicitConversions
import scala.annotation.nowarn
import scalus.*
import scalus.prelude.{*, given}
import scalus.prelude.Eq.given
import scalus.prelude.Ord.{*, given}
import scalus.uplc.*
import scalus.uplc.eval.*
import org.scalatest.funsuite.AnyFunSuite

@Compile
object Min202507171 {

    enum Tree:
        case One(arg: BigInt)
        case Two(arg1: Tree, arg2: Tree)

    def doClause(
        formula: Tree,
        vars: (List[BigInt], List[BigInt])
    ): (List[BigInt], List[BigInt]) =
        formula match
            case Tree.Two(arg1, arg2) => doClause(arg1, doClause(arg2, vars))
            case _                    => fail()

}

@Compile
object Min202507172:

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

end Min202507172

@Compile
object ClausifyTest:
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

    extension (self: Int)
        inline infix def <->(other: Int): Formula = Equivalence(Sym(self), Sym(other))
        inline infix def <->(other: Formula): Formula = Equivalence(Sym(self), other)

    end extension

    extension (self: Var) inline def toFormula: Formula = Sym(self)
    extension (self: Int) @Ignore inline def toFormula: Formula = Sym(self)

    @nowarn
    @Ignore
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
                def isAnd: Boolean = self match {
                    case And(_, _) => true;
                    case _         => false
                }

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

end ClausifyTest

class ClausifyMinTest extends AnyFunSuite:

    given PlutusVM = PlutusVM.makePlutusV3VM()

    @Ignore
    inline given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    test("fail as foirst argument in branch") {
        import Min202507171.*
        val sir = Compiler
            .compile {

                val x = doClause(Tree.One(BigInt(1)), (List.empty, List.empty))

            }
        val uplc = sir.toUplc(generateErrorTraces = false)

        val result = uplc.evaluateDebug

        assert(result.isFailure)

    }

    test("insertUniqueOrdered should work") {
        import Min202507172.*
        val sir = Compiler
            .compile {
                val lrVars: (List[BigInt], List[BigInt]) = (List.single(BigInt(1)), List.empty)
                List.empty.insertUniqueOrdered(lrVars)
            }
        val uplc = sir.toUplcOptimized(generateErrorTraces = false)

        val result = uplc.evaluateDebug

        assert(result.isSuccess)

    }

    test("F3") {
        import ClausifyTest.*
        val result = Compiler
            .compile {
                // (a = a = a) = (a = a) = (a = a)
                val formula = (1 <-> (1 <-> 1)) <-> ((1 <-> 1) <-> (1 <-> 1))
                val expected = List.single[LRVars]((List.single[Var](1), List.empty[Var]))
                require(formula.clauses === expected)
            }
            .toUplcOptimized(false)
            .evaluateDebug

        assert(result.isSuccess)

    }

end ClausifyMinTest
