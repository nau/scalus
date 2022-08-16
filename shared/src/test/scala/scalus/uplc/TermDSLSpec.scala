package scalus.uplc

import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.{Checkers, ScalaCheckPropertyChecks}
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.TermDSL.{*, given}

object ScalaJSExampleSpec extends Properties("ScalaCheck-scalajs") with ArbitraryInstances {

  property("dummy") = Prop.forAll { (t: Term) =>
    Prop.classify(t.isInstanceOf[Term.Const], "const") {
      Prop.classify(t.isInstanceOf[Term.Force] || t.isInstanceOf[Term.Delay], "force/delay") {
        Prop.classify(
          t.isInstanceOf[Term.LamAbs] || t.isInstanceOf[Term.Apply] || t.isInstanceOf[Term.Var],
          "var/lam/apply"
        ) {
          Prop.classify(t.isInstanceOf[Term.Builtin], "builtin") {
            Prop.classify(t.isInstanceOf[Term.Error.type], "error") {
              t == t
            }
          }
        }
      }
    }
  }
}

class TermDSLSpec extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances:
  test("constant as Constant") {
    val int: Constant = asConstant(2)
    assert(int == Constant.Integer(2))
    val bigint: Constant = asConstant(BigInt(2))
    assert(bigint == Constant.Integer(2))
    val bool: Constant = asConstant(true)
    assert(bool == Constant.Bool(true))
    val ba: Constant = asConstant(Array[Byte](2, 3))
    assert(ba == Constant.ByteString(Array[Byte](2, 3)))
    val s: Constant = asConstant("Hello")
    assert(s == Constant.String("Hello"))
    val u: Constant = asConstant(())
    assert(u == Constant.Unit)
    val li: Constant = asConstant(Seq(1, 2))
    assert(
      li ==
        Constant.List(DefaultUni.Integer, Constant.Integer(1) :: Constant.Integer(2) :: Nil)
    )
    val p: Constant = asConstant((1, false))
    assert(
      p ==
        Constant.Pair(
          Constant.Integer(1),
          Constant.Bool(false)
        )
    )
  }
  test("constant as Term") {
    val int: Term = 2
    assert(int == Term.Const(Constant.Integer(2)))
    val bigint: Term = BigInt(2)
    assert(bigint == Term.Const(Constant.Integer(2)))
    val bool: Term = true
    assert(bool == Term.Const(Constant.Bool(true)))
    val ba: Term = Array[Byte](2, 3)
    assert(ba == Term.Const(Constant.ByteString(Array[Byte](2, 3))))
    val s: Term = "Hello"
    assert(s == Term.Const(Constant.String("Hello")))
    val u: Term = ()
    assert(u == Term.Const(Constant.Unit))
    val li: Term = Seq(1, 2)
    assert(
      li == Term.Const(
        Constant.List(DefaultUni.Integer, Constant.Integer(1) :: Constant.Integer(2) :: Nil)
      )
    )
    val p: Term = (1, false)
    assert(
      p == Term.Const(
        Constant.Pair(
          Constant.Integer(1),
          Constant.Bool(false)
        )
      )
    )
  }

  test("exists an implicit conversion from Constant to Term") {
    val int: Term = Constant.Integer(2)
    assert(int == Term.Const(Constant.Integer(2)))
  }

  test("force/delay/λ/apply") {
    forAll { (t: Term) =>
      assert(!t == Term.Force(t))
      assert(~t == Term.Delay(t))
      assert((t $ t) == Term.Apply(t, t))
      assert(λ("x")(t) == Term.LamAbs("x", t))
      assert(lam("x")(t) == Term.LamAbs("x", t))
    }
  }
