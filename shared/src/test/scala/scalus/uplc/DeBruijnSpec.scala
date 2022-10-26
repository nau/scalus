package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.uplc.DefaultUni.{ByteString, asConstant}
import scalus.uplc.Term.*

import java.io.ByteArrayInputStream
import scala.io.Source.fromFile

class DeBruijnSpec extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances:
  test("deBruijnTerm") {
    import TermDSL.*
    val t = lam("x", "x", "y")(Var(NamedDeBruijn("x")))
    val deBruijnedTerm = DeBruijn.deBruijnTerm(t)
    assert(deBruijnedTerm == LamAbs("x", LamAbs("x", LamAbs("y", Var(NamedDeBruijn("x", 2))))))
  }

  test("fromDeBruijnTerm") {
    import TermDSL.*
    val t = lam("x", "x", "y")(Var(NamedDeBruijn("x")))
    val deBruijnedTerm = DeBruijn.deBruijnTerm(t)
    val namedTerm = DeBruijn.fromDeBruijnTerm(deBruijnedTerm)
    assert(deBruijnedTerm == LamAbs("x", LamAbs("x", LamAbs("y", Var(NamedDeBruijn("x", 2))))))
    assert(namedTerm == LamAbs("i0", LamAbs("i1", LamAbs("i2", Var(NamedDeBruijn("i1", 2))))))
  }

  test("fromDeBruijnTerm(deBruijnTerm(t)) == t") {
    forAll { (t: Term) =>
      val deBruijnedTerm = DeBruijn.deBruijnTerm(t)
      val namedTerm = DeBruijn.fromDeBruijnTerm(deBruijnedTerm)
      val deBruijnedTerm2 = DeBruijn.deBruijnTerm(namedTerm)
      val namedTerm2 = DeBruijn.fromDeBruijnTerm(deBruijnedTerm2)
      assert(namedTerm == namedTerm2)
    }
  }
