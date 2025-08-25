package scalus.prelude

import scalus.*
import scalus.uplc.{Constant, Term}
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.uplc.test.ArbitraryInstances
import scalus.sir.{AnnotationsDecl, SIR, SIRType}
import scalus.sir.SirDSL.$
import scalus.builtin.Data
import scalus.builtin.Data.{fromData, toData, FromData, ToData}
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.util.Pretty
import org.scalactic.{source, Prettifier}

import scala.util.control.NonFatal
import scala.reflect.ClassTag

class StdlibTestKit extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances {
    export org.scalatestplus.scalacheck.Checkers.*
    export org.scalacheck.{Arbitrary, Gen, Shrink}
    // export scalus.builtin.Data
    // export scalus.builtin.Data.{fromData, toData, FromData, ToData}
    export scalus.prelude.{!==, ===}
    // export scalus.prelude.{Eq, Ord}
    export Ord.{<=>, Order}

    inline given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

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

    protected final inline def checkEvalFails[E <: Throwable: ClassTag](inline code: Any): Unit = {
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

    protected inline final def checkEval[A1](
        inline f: A1 => Boolean,
        configParams: org.scalatestplus.scalacheck.Checkers.PropertyCheckConfigParam*
    )(implicit
        inline a1FromData: FromData[A1],
        inline a1ToData: ToData[A1],
        config: PropertyCheckConfiguration,
        a1: Arbitrary[A1],
        s1: Shrink[A1],
        pp1: A1 => Pretty,
        prettifier: Prettifier,
        pos: source.Position
    ): Assertion = {
        import scala.compiletime.summonInline

        val sir = Compiler.compileInline { (data: Data) => f(fromData[A1](data)) }
        val uplc = sir.toUplc(true)

        def handler(payload: A1): Boolean = {
            // val applied =
            //    sir $ SIR.Const(Constant.Data(payload.toData), SIRType.Data, AnnotationsDecl.empty)
            val applied = uplc $ Term.Const(Constant.Data(toData[A1](payload)))
            val resultTerm = applied.evaluate
            Term.alphaEq(resultTerm, trueTerm) && f(payload)
        }

        check(handler, configParams*)
    }

    protected inline final def checkEval[A1, A2](
        inline f: (A1, A2) => Boolean,
        configParams: org.scalatestplus.scalacheck.Checkers.PropertyCheckConfigParam*
    )(implicit
        inline a1FromData: FromData[A1],
        inline a1ToData: ToData[A1],
        inline a2FromData: FromData[A2],
        inline a2ToData: ToData[A2],
        config: PropertyCheckConfiguration,
        a1: Arbitrary[A1],
        s1: Shrink[A1],
        pp1: A1 => Pretty,
        a2: Arbitrary[A2],
        s2: Shrink[A2],
        pp2: A2 => Pretty,
        prettifier: Prettifier,
        pos: source.Position
    ): Assertion = {
        val sir = Compiler.compileInline { (d1: Data, d2: Data) =>
            f(fromData[A1](d1), fromData[A2](d2))
        }

        val uplc = sir.toUplc(true)

        def handler(payload1: A1, payload2: A2): Boolean = {
            val applied = uplc $ Term.Const(Constant.Data(payload1.toData)) $ Term.Const(
              Constant.Data(payload2.toData)
            )
            val resultTerm = applied.evaluate
            Term.alphaEq(resultTerm, trueTerm) && f(payload1, payload2)
        }

        check(handler, configParams*)
    }

    protected inline final def checkEval[A1, A2, A3](
        inline f: (A1, A2, A3) => Boolean,
        configParams: org.scalatestplus.scalacheck.Checkers.PropertyCheckConfigParam*
    )(implicit
        inline a1FromData: FromData[A1],
        inline a1ToData: ToData[A1],
        inline a2FromData: FromData[A2],
        inline a2ToData: ToData[A2],
        inline a3FromData: FromData[A3],
        inline a3ToData: ToData[A3],
        config: PropertyCheckConfiguration,
        a1: Arbitrary[A1],
        s1: Shrink[A1],
        pp1: A1 => Pretty,
        a2: Arbitrary[A2],
        s2: Shrink[A2],
        pp2: A2 => Pretty,
        a3: Arbitrary[A3],
        s3: Shrink[A3],
        pp3: A3 => Pretty,
        prettifier: Prettifier,
        pos: source.Position
    ): Assertion = {
        val sir = Compiler.compileInline { (d1: Data, d2: Data, d3: Data) =>
            f(fromData[A1](d1), fromData[A2](d2), fromData[A3](d3))
        }

        val uplc = sir.toUplc(true)

        def handler(payload1: A1, payload2: A2, payload3: A3): Boolean = {
            val applied = uplc $ Term.Const(Constant.Data(payload1.toData)) $ Term.Const(
              Constant.Data(payload2.toData)
            ) $ Term.Const(Constant.Data(payload3.toData))
            val resultTerm = applied.evaluate
            Term.alphaEq(resultTerm, trueTerm) && f(payload1, payload2, payload3)
        }

        check(handler, configParams*)
    }

    protected inline final def checkEval[A1, A2, A3, A4](
        inline f: (A1, A2, A3, A4) => Boolean,
        configParams: org.scalatestplus.scalacheck.Checkers.PropertyCheckConfigParam*
    )(implicit
        inline a1FromData: FromData[A1],
        inline a1ToData: ToData[A1],
        inline a2FromData: FromData[A2],
        inline a2ToData: ToData[A2],
        inline a3FromData: FromData[A3],
        inline a3ToData: ToData[A3],
        inline a4FromData: FromData[A4],
        inline a4ToData: ToData[A4],
        config: PropertyCheckConfiguration,
        a1: Arbitrary[A1],
        s1: Shrink[A1],
        pp1: A1 => Pretty,
        a2: Arbitrary[A2],
        s2: Shrink[A2],
        pp2: A2 => Pretty,
        a3: Arbitrary[A3],
        s3: Shrink[A3],
        pp3: A3 => Pretty,
        a4: Arbitrary[A4],
        s4: Shrink[A4],
        pp4: A4 => Pretty,
        prettifier: Prettifier,
        pos: source.Position
    ): Assertion = {
        val sir = Compiler.compileInline { (d1: Data, d2: Data, d3: Data, d4: Data) =>
            f(fromData[A1](d1), fromData[A2](d2), fromData[A3](d3), fromData[A4](d4))
        }

        val uplc = sir.toUplc(true)

        def handler(payload1: A1, payload2: A2, payload3: A3, payload4: A4): Boolean = {
            val applied = uplc $ Term.Const(Constant.Data(payload1.toData)) $ Term.Const(
              Constant.Data(payload2.toData)
            ) $ Term.Const(Constant.Data(payload3.toData)) $ Term.Const(
              Constant.Data(payload4.toData)
            )
            val resultTerm = applied.evaluate
            Term.alphaEq(resultTerm, trueTerm) && f(payload1, payload2, payload3, payload4)
        }

        check(handler, configParams*)
    }

    protected inline final def checkEval[A1, A2, A3, A4, A5](
        inline f: (A1, A2, A3, A4, A5) => Boolean,
        configParams: org.scalatestplus.scalacheck.Checkers.PropertyCheckConfigParam*
    )(implicit
        inline a1FromData: FromData[A1],
        inline a1ToData: ToData[A1],
        inline a2FromData: FromData[A2],
        inline a2ToData: ToData[A2],
        inline a3FromData: FromData[A3],
        inline a3ToData: ToData[A3],
        inline a4FromData: FromData[A4],
        inline a4ToData: ToData[A4],
        inline a5FromData: FromData[A5],
        inline a5ToData: ToData[A5],
        config: PropertyCheckConfiguration,
        a1: Arbitrary[A1],
        s1: Shrink[A1],
        pp1: A1 => Pretty,
        a2: Arbitrary[A2],
        s2: Shrink[A2],
        pp2: A2 => Pretty,
        a3: Arbitrary[A3],
        s3: Shrink[A3],
        pp3: A3 => Pretty,
        a4: Arbitrary[A4],
        s4: Shrink[A4],
        pp4: A4 => Pretty,
        a5: Arbitrary[A5],
        s5: Shrink[A5],
        pp5: A5 => Pretty,
        prettifier: Prettifier,
        pos: source.Position
    ): Assertion = {
        val sir = Compiler.compileInline { (d1: Data, d2: Data, d3: Data, d4: Data, d5: Data) =>
            f(
              fromData[A1](d1),
              fromData[A2](d2),
              fromData[A3](d3),
              fromData[A4](d4),
              fromData[A5](d5)
            )
        }

        val uplc = sir.toUplc(true)

        def handler(
            payload1: A1,
            payload2: A2,
            payload3: A3,
            payload4: A4,
            payload5: A5
        ): Boolean = {
            val applied = uplc $ Term.Const(Constant.Data(payload1.toData)) $ Term.Const(
              Constant.Data(payload2.toData)
            ) $ Term.Const(Constant.Data(payload3.toData)) $ Term.Const(
              Constant.Data(payload4.toData)
            ) $ Term.Const(Constant.Data(payload5.toData))
            val resultTerm = applied.evaluate
            Term.alphaEq(resultTerm, trueTerm) && f(
              payload1,
              payload2,
              payload3,
              payload4,
              payload5
            )
        }

        check(handler, configParams*)
    }

    protected inline final def checkEval[A1, A2, A3, A4, A5, A6](
        inline f: (A1, A2, A3, A4, A5, A6) => Boolean,
        configParams: org.scalatestplus.scalacheck.Checkers.PropertyCheckConfigParam*
    )(implicit
        inline a1FromData: FromData[A1],
        inline a1ToData: ToData[A1],
        inline a2FromData: FromData[A2],
        inline a2ToData: ToData[A2],
        inline a3FromData: FromData[A3],
        inline a3ToData: ToData[A3],
        inline a4FromData: FromData[A4],
        inline a4ToData: ToData[A4],
        inline a5FromData: FromData[A5],
        inline a5ToData: ToData[A5],
        inline a6FromData: FromData[A6],
        inline a6ToData: ToData[A6],
        config: PropertyCheckConfiguration,
        a1: Arbitrary[A1],
        s1: Shrink[A1],
        pp1: A1 => Pretty,
        a2: Arbitrary[A2],
        s2: Shrink[A2],
        pp2: A2 => Pretty,
        a3: Arbitrary[A3],
        s3: Shrink[A3],
        pp3: A3 => Pretty,
        a4: Arbitrary[A4],
        s4: Shrink[A4],
        pp4: A4 => Pretty,
        a5: Arbitrary[A5],
        s5: Shrink[A5],
        pp5: A5 => Pretty,
        a6: Arbitrary[A6],
        s6: Shrink[A6],
        pp6: A6 => Pretty,
        prettifier: Prettifier,
        pos: source.Position
    ): Assertion = {
        val sir = Compiler.compileInline {
            (d1: Data, d2: Data, d3: Data, d4: Data, d5: Data, d6: Data) =>
                f(
                  fromData[A1](d1),
                  fromData[A2](d2),
                  fromData[A3](d3),
                  fromData[A4](d4),
                  fromData[A5](d5),
                  fromData[A6](d6)
                )
        }

        val uplc = sir.toUplc(true)

        def handler(
            payload1: A1,
            payload2: A2,
            payload3: A3,
            payload4: A4,
            payload5: A5,
            payload6: A6
        ): Boolean = {
            val applied = uplc $ Term.Const(Constant.Data(payload1.toData)) $ Term.Const(
              Constant.Data(payload2.toData)
            ) $ Term.Const(Constant.Data(payload3.toData)) $ Term.Const(
              Constant.Data(payload4.toData)
            ) $ Term.Const(Constant.Data(payload5.toData)) $ Term.Const(
              Constant.Data(payload6.toData)
            )
            val resultTerm = applied.evaluate
            Term.alphaEq(resultTerm, trueTerm) && f(
              payload1,
              payload2,
              payload3,
              payload4,
              payload5,
              payload6
            )
        }

        check(handler, configParams*)
    }

    private val trueTerm = Compiler.compileInline(true).toUplc(true).evaluate
    protected given PlutusVM = PlutusVM.makePlutusV3VM()
}
