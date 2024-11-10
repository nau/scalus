package scalus
package uplc

import scalus.uplc.Term.*
import scalus.uplc.TermDSL.{*, given}
import scalus.uplc.Constant.given
import DefaultFun.*
import org.scalatest.funsuite.AnyFunSuite
import scala.language.implicitConversions

class InlineIdentitySpec extends AnyFunSuite {
    test("inlineIdentity should inline identity function application") {
        val term = Apply(LamAbs("x", Var(NamedDeBruijn("x"))), Var(NamedDeBruijn("y")))
        val expected = Var(NamedDeBruijn("y"))
        assert(Inliner.inlinePass(term) == expected)
    }

    test("inlineIdentity should inline identity Var") {
        val term = LamAbs("x", vr"x" $ vr"x") $ vr"y"
        val expected = vr"y" $ vr"y"
        assert(Inliner.inlinePass(term) == expected)
    }

    test("constants should remain unchanged") {
        val constTerm: Term = 42
        assert(Inliner.inlinePass(constTerm) == constTerm)

        val strTerm: Term = "hello"
        assert(Inliner.inlinePass(strTerm) == strTerm)
    }

    test("builtins should remain unchanged") {
        val addTerm: Term = AddInteger
        assert(Inliner.inlinePass(addTerm) == addTerm)

        val mulTerm: Term = MultiplyInteger
        assert(Inliner.inlinePass(mulTerm) == mulTerm)
    }

    test("identity function should be eliminated") {
        // (λx.x) 42 => 42
        val term = λ("x")(vr"x") $ 42
        assert(Inliner.inlinePass(term) == Const(Constant.Integer(42)))

        // (λx.x) "hello" => "hello"
        val strTerm = λ("x")(vr"x") $ "hello"
        assert(Inliner.inlinePass(strTerm) == Const(Constant.String("hello")))
    }

    test("nested identity functions should all be eliminated") {
        // (λx.x) ((λy.y) 42) => 42
        val term = λ("x")(vr"x") $ (λ("y")(vr"y") $ 42)
        assert(Inliner.inlinePass(term) == Const(Constant.Integer(42)))
    }

    test("variable substitution should work correctly") {
        // (λx. x + x) 42 => 42 + 42
        val term = λ("x")(AddInteger $ vr"x" $ vr"x") $ 42

        val expected = AddInteger $ 42 $ 42

        assert(Inliner.inlinePass(term) == expected)
    }

    test("should avoid name capture through alpha-renaming") {
        // (λx. λy. x) y
        // Should alpha-rename y in the inner lambda to avoid capture
        val term = λ("x", "y")(vr"x") $ vr"y"

        val result = Inliner.inlinePass(term)

        // The result should be λy_1. y where y_1 is a fresh name
        result match
            case LamAbs(newName, Var(NamedDeBruijn("y", 0))) =>
                assert(newName != "y")
            case _ =>
                fail(s"Unexpected result: $result")
    }

    test("should handle Force/Delay correctly") {
        val term = !(~42)
        assert(Inliner.inlinePass(term) == term)

        // Test with more complex expressions
        val complexTerm = !(~(AddInteger $ 42 $ 21))
        assert(Inliner.inlinePass(complexTerm) == complexTerm)
    }

    test("should handle Constr and Case") {
        val constr = Constr(0, List(42))
        assert(Inliner.inlinePass(constr) == constr)

        val caseExpr = Case(
          constr,
          List(
            λ("x")(vr"x") $ vr"y", // Identity function application
            0
          )
        )

        val expectedCase = Case(
          constr,
          List(
            vr"y", // Identity function eliminated
            0
          )
        )

        assert(Inliner.inlinePass(caseExpr) == expectedCase)
    }

    test("should not inline non-pure terms") {
        // (λx. x + x) Error => (λx. x + x) Error
        val termWithError = λ("x")(AddInteger $ vr"x" $ vr"x") $ Error
        assert(Inliner.inlinePass(termWithError) == termWithError)
    }

    test("should handle complex arithmetic expressions") {
        // (λx. λy. x + y) 42 21
        val term = λ("x", "y")(AddInteger $ vr"x" $ vr"y") $ 42 $ 21
        val expected = AddInteger $ 42 $ 21
        assert(Inliner.inlinePass(term) == expected)
    }

    test("should properly handle substitution with potential capture") {
        // (λx.λy.x) y => λy'.y
        val term = λ("x")(λ("y")(vr"x")) $ vr"y"
        val result = Inliner.inlinePass(term)

        result match
            case LamAbs(newName, Var(NamedDeBruijn("y", 0))) =>
                assert(newName != "y") // Should be renamed to avoid capture
            case _ => fail(s"Unexpected result: $result")
    }

    test("should handle substitution with multiple bound variables") {
        // (λx.λy.x y) y => λy'.y y
        val term = λ("x")(λ("y")(vr"x" $ vr"y")) $ vr"y"
        val result = Inliner.inlinePass(term)

        result match
            case LamAbs(newName, Apply(Var(NamedDeBruijn("y", 0)), Var(NamedDeBruijn(y2, 0)))) =>
                assert(newName != "y") // Should be renamed
                assert(y2 == newName) // The bound y should refer to the new name
            case _ => fail(s"Unexpected result: $result")
    }

    test("should respect shadowing in substitution") {
        // (λx.λx.x) y => λx.x
        val term = λ("x")(λ("x")(vr"x")) $ vr"y"
        val result = Inliner.inlinePass(term)

        assert(
          result == λ("x")(vr"x")
        ) // The inner x shadows outer x, so y shouldn't be substituted
    }

    test("should handle multiple variable references") {
        // (λx. x + (x * x)) 42 => 42 + (42 * 42)
        val term = λ("x")(
          AddInteger $ vr"x" $ (MultiplyInteger $ vr"x" $ vr"x")
        ) $ 42

        val expected = AddInteger $ 42 $ (MultiplyInteger $ 42 $ 42)
        assert(Inliner.inlinePass(term) == expected)
    }
}
