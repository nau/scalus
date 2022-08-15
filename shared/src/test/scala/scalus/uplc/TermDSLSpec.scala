package scalus.uplc

import org.scalacheck.Prop
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.uplc.TermDSL.{*, given}

class TermDSLSpec extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances:
  test("constant as Term") {
    val int: Term = 2
    assert(int == Term.Const(Constant(DefaultUni.Integer, 2)))
    val bigint: Term = BigInt(2)
    assert(bigint == Term.Const(Constant(DefaultUni.Integer, 2)))
    val bool: Term = true
    assert(bool == Term.Const(Constant(DefaultUni.Bool, true)))
    val ba: Term = Array[Byte](2, 3)
    assert(ba == Term.Const(Constant(DefaultUni.ByteString, Array[Byte](2, 3))))
    val s: Term = "Hello"
    assert(s == Term.Const(Constant(DefaultUni.String, "Hello")))
    val u: Term = ()
    assert(u == Term.Const(Constant(DefaultUni.Unit, ())))
  }

  test("force/delay/λ/apply") {
    forAll { (t: Term) =>
      assert(!t == Term.Force(t))
      assert(~t == Term.Delay(t))
      assert((t $ t) == Term.Apply(t, t))
      assert(λ("x")(t) == Term.LamAbs("x", t))
    }
  }
