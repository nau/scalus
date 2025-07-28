package scalus.prelude

import scalus.*
import scalus.uplc.Term
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.uplc.test.ArbitraryInstances

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.util.control.NonFatal
import scala.reflect.ClassTag

export org.scalacheck.{Arbitrary, Gen}

class StdlibTestKit extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances {
    export org.scalatestplus.scalacheck.Checkers.*
    export scalus.builtin.Data.{fromData, toData}
    export Eq.given
    export Ord.*

    protected final inline def liftThrowableToOption[A](inline code: A): Option[A] = {
        try Option.Some(code)
        catch case NonFatal(exception) => Option.None
    }

    protected final inline def assertEvalFails[E <: Throwable: ClassTag](inline code: Any): Unit = {
        var isExceptionThrown = false

        val _ =
            try code
            catch
                case NonFatal(exception) =>
                    assert(
                      ClassTag(exception.getClass) == summon[ClassTag[E]],
                      s"Expected exception of type ${summon[ClassTag[E]]}, but got $exception"
                    )
                    val result = Compiler.compileInline(code).toUplc(true).evaluateDebug
                    result match
                        case failure: Result.Failure =>
                            val thrown = failure.exception
                            if thrown != exception then {
                                alert(
                                  "exception thrown by the UPLC evaluator did not equal or contain the expected exception."
                                )
                            }
                        case _ =>
                            fail(s"Expected failure, but got success: $result")

                    isExceptionThrown = true

        if !isExceptionThrown then
            fail(s"Expected exception of type ${summon[ClassTag[E]]}, but got success: $code")
    }

    protected final inline def assertEvalEq[T: Eq](inline code: T, inline expected: T): Unit = {
        assert(code === expected, s"Expected $expected, but got $code")

        val codeTerm = Compiler.compileInline(code).toUplc(true).evaluate
        val expectedTerm = Compiler.compileInline(expected).toUplc(true).evaluate
        assert(
          Term.alphaEq(codeTerm, expectedTerm),
          s"Expected term $expectedTerm, but got $codeTerm"
        )
    }

    protected final inline def assertEvalNotEq[T: Eq](inline code: T, inline expected: T): Unit = {
        assert(code !== expected, s"Expected not equal to $expected, but got $code")

        val codeTerm = Compiler.compileInline(code).toUplc(true).evaluate
        val expectedTerm = Compiler.compileInline(expected).toUplc(true).evaluate
        assert(
          !Term.alphaEq(codeTerm, expectedTerm),
          s"Expected term not equal to $expectedTerm, but got $codeTerm"
        )
    }

    protected final inline def assertEval(inline code: Boolean): Unit = {
        assert(code)

        val codeTerm = Compiler.compileInline(code).toUplc(true).evaluate
        assert(Term.alphaEq(codeTerm, trueTerm))
    }

    private val trueTerm = Compiler.compileInline(true).toUplc(true).evaluate
    private given PlutusVM = PlutusVM.makePlutusV3VM()
}
