package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.given
import scalus.builtin.Builtins.*
import scalus.builtin.Data
import scalus.flat.Flat
import scalus.uplc.FlatInstantces.given
import scalus.uplc.NamedDeBruijn
import scalus.uplc.Term
import scalus.uplc.Term.*
import scalus.uplc.eval.ExBudget.given
import scala.math.Ordering.Implicits.*

class ExprSizeAndBudgetSpec extends AnyFunSuite {
    private val encoder = summon[Flat[Term]]
    private val boolSize = encoder.bitSize(compile(true).toUplc())
    private val unitSize = encoder.bitSize(compile(()).toUplc())
    private val fun1Uplc = compile((b: Boolean) => b).toUplc()
    private val fun1Size = encoder.bitSize(fun1Uplc)

    test("unit bit size is 10") {
        assert(unitSize == 10)
    }

    test("bool bit size is 11") {
        assert(boolSize == 11)
    }

    test("BigInt bit size is 26") {
        assert(encoder.bitSize(compile(BigInt(123)).toUplc()) == 26)
    }

    test("Var bit size is 12") {
        assert(encoder.bitSize(Term.Var(NamedDeBruijn("a", 1))) == 12)
    }

    test("fun1 bit size is 16") {
        assert(fun1Size == 16)
    }

    test("let bit size is 8") {
        val uplc = compile { val a = true }.toUplc()
        assert(encoder.bitSize(uplc) - unitSize - boolSize == 8)
    }

    test("new prelude.List.Cons(true, prelude.List.Nil) size is 83") {
        val uplc = compile(new prelude.List.Cons(true, prelude.List.Nil)).toUplcOptimized()
        assert(encoder.bitSize(uplc) == 83)
    }

    test("prelude.List.cons(true, prelude.List.Nil) size is 123") {
        val uplc = compile(prelude.List.single(true)).toUplcOptimized()
        assert(encoder.bitSize(uplc) == 123)
    }

    test("prelude.List.single(true) size is 123") {
        val uplc = compile(prelude.List.single(true)).toUplcOptimized()
        assert(encoder.bitSize(uplc) == 123)
    }

    test("equalsInteger(unIData) < equalsData(iData)") {
        given PlutusVM = PlutusVM.makePlutusV3VM()
        val eqDataSIR = compile { (d: Data) => equalsData(d, iData(1)) }
        val eqIntegerSIR = compile { (d: Data) => equalsInteger(unIData(d), 1) }
        val d = iData(1)
        val eqDataUplc = eqDataSIR.toUplc() $ d.asTerm
        val eqIntegerUplc = eqIntegerSIR.toUplc() $ d.asTerm
        val (eqData, eqInteger) = (eqDataUplc.evaluateDebug, eqIntegerUplc.evaluateDebug)
        // here we ensure that it's cheaper to convert the data to an integer and compare with equalsInteger
        // than to convert the integer to data and compare with equalsData
        // this is a prerequisite for the optimizations to be valid
        assert(eqInteger.budget < eqData.budget)
    }

}
