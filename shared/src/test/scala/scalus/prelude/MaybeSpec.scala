package scalus.prelude

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Properties
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.pretty
import scalus.uplc.ArbitraryInstances

import scala.collection.immutable

import scalus.prelude.Prelude.{*, given}
import scalus.prelude.Maybe.*
import scalus.Compiler.compile
import scalus.sir.SIR
import scalus.uplc.Term
import scalus.uplc.TermDSL.{*, given}
import scalus.sir.SimpleSirToUplcLowering
import scalus.uplc.Cek

class MaybeSpec extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances {
  test("eq") {
    assert((Nothing: Maybe[String]) === Nothing)
    assert(Just(BigInt(1)) === Just(BigInt(1)))
    assert(Just(BigInt(1)) !== Just(BigInt(2)))
    assertEval(compile(new Just(true) === Nothing), false)
    assertEval(compile(new Just(true) === new Just(true)), true)
    assertEval(compile(new Just(true) !== new Just(true)), false)
  }

  private def assertEval(sir: SIR, expected: Term) = {
    val term = new SimpleSirToUplcLowering().lower(sir)
    assert(Cek.evalUPLC(term) == expected)
  }
}
