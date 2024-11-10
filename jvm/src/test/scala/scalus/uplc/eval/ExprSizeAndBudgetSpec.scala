package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.flat.Flat
import scalus.uplc.FlatInstantces.given
import scalus.uplc.NamedDeBruijn
import scalus.uplc.Term

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

    test("new prelude.List.Cons(true, prelude.List.Nil) size is 103") {
        val uplc = compile(new prelude.List.Cons(true, prelude.List.Nil)).toUplcOptimized()
        assert(encoder.bitSize(uplc) == 83)
    }

    test("prelude.List.cons(true, prelude.List.Nil) size is 123") {
        val uplc = compile(prelude.List.cons(true, prelude.List.Nil)).toUplcOptimized()
        assert(encoder.bitSize(uplc) == 123)
    }

    test("prelude.List.single(true) size is 143") {
        val uplc = compile(prelude.List.single(true)).toUplcOptimized()
        assert(encoder.bitSize(uplc) == 123)
    }
}
