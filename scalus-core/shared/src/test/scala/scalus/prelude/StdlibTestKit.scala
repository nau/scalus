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
    export scalus.prelude.{Eq, Ord}
    export Eq.given
    export scalus.prelude.{!==, ===}
    export Ord.{<=>, Order}

    protected final inline def liftThrowableToOption[A](inline code: A): Option[A] = {
        try Option.Some(code)
        catch case NonFatal(_) => Option.None
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
                            result.logs.lastOption match {
                                case Some(message) =>
                                    assert(message.contains(exception.getMessage))
                                case None =>
                                    // if the error occurred due to an erroneously called builtin, e.g. / by zero,
                                    // there won't be a respective log, but the CEK exception message is going to include
                                    // the root error.
                                    assert(
                                      failure.exception.getMessage.contains(
                                        exception.getClass.getName
                                      )
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
