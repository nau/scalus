package scalus.prelude

import scalus.*
import org.scalacheck.Arbitrary
import scalus.uplc.test.ArbitraryInstances
import org.scalacheck.Prop
import org.scalatestplus.scalacheck.Checkers.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.{ImpossibleLedgerStateError, OnchainError, RequirementError}
import scalus.uplc.Constant.toValue
import scalus.uplc.{Constant, Term}
import scalus.uplc.eval.{PlutusVM, Result}
import scala.reflect.ClassTag
import Eq.given

class SortedMapTest extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    test("empty") {
        assertEvalEq(SortedMap.empty[BigInt, BigInt].toList, List.empty[(BigInt, BigInt)])
        assertEvalEq(SortedMap.empty[BigInt, BigInt].size, BigInt(0))
        assertEvalEq(SortedMap.empty[BigInt, BigInt].length, BigInt(0))
        assertEvalEq(SortedMap.empty[BigInt, BigInt].isEmpty, true)
        assertEvalEq(SortedMap.empty[BigInt, BigInt].nonEmpty, false)
        assertEvalFails[NoSuchElementException](
          SortedMap.empty[BigInt, BigInt].getOrFail(BigInt(0))
        )
    }

    private inline def assertEvalFails[E <: Throwable: ClassTag](inline code: Any): Unit = {
        assertThrows[E](code)

        val result = Compiler.compileInline(code).toUplc(true).evaluateDebug
        assert(result.isFailure)
    }

    private inline def assertEvalEq[T: Eq](inline code: T, inline expected: T): Unit = {
        assert(code === expected, s"Expected $expected, but got $code")

        val codeTerm = Compiler.compileInline(code).toUplc(true).evaluate
        val expectedTerm = Compiler.compileInline(expected).toUplc(true).evaluate
        assert(codeTerm == expectedTerm, s"Expected term $expectedTerm, but got $codeTerm")
    }
}
