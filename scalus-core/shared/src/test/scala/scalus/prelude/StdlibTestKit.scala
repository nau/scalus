package scalus.prelude

import scalus.*
import scalus.uplc.Term
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.uplc.test.ArbitraryInstances

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.reflect.ClassTag
import scala.util.control.NonFatal

class StdlibTestKit extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances {
    export org.scalatestplus.scalacheck.Checkers.*
    export Eq.given
    export Ord.*
    export scalus.builtin.Data.{fromData, toData}

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
                            val errorMessage = result.logs.last

                            assert(
                              errorMessage.contains(exception.getMessage),
                              s"Expected error message '${exception.getMessage}', but got '$errorMessage'"
                            )
                        case _ =>
                            fail(s"Expected failure, but got success: $result")

                    isExceptionThrown = true

        if !isExceptionThrown then
            fail(s"Expected exception of type ${summon[ClassTag[E]]}, but no exception was thrown")
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

    protected final inline def assertEval(inline code: Boolean): Unit = {
        assert(code)

        val codeTerm = Compiler.compileInline(code).toUplc(true).evaluate
        assert(Term.alphaEq(codeTerm, trueTerm))
    }

    private val trueTerm = Compiler.compileInline(true).toUplc(true).evaluate
    private given PlutusVM = PlutusVM.makePlutusV3VM()
}
