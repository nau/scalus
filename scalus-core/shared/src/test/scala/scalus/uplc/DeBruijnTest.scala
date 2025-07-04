package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.uplc.Term.*
import scalus.uplc.test.ArbitraryInstances

class DeBruijnTest extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances:
    test("deBruijnTerm") {
        val t = lam("x", "x", "y")(Var(NamedDeBruijn("x")))
        val deBruijnedTerm = DeBruijn.deBruijnTerm(t)
        assert(deBruijnedTerm == LamAbs("x", LamAbs("x", LamAbs("y", Var(NamedDeBruijn("x", 2))))))
    }

    test("fromDeBruijnTerm") {
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
