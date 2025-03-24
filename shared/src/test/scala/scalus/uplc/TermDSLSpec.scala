package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.builtin.ByteString
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given

import scala.language.implicitConversions

class TermDSLSpec extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances:
    test("constant as Constant") {
        val int: Constant = asConstant(2)
        assert(int == Constant.Integer(2))
        val bigint: Constant = asConstant(BigInt(2))
        assert(bigint == Constant.Integer(2))
        val bool: Constant = asConstant(true)
        assert(bool == Constant.Bool(true))
        val ba: Constant = asConstant(ByteString(2, 3))
        assert(ba == Constant.ByteString(ByteString(2, 3)))
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
        val ba: Term = ByteString(2, 3)
        assert(ba == Term.Const(Constant.ByteString(ByteString(2, 3))))
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

    test("force/delay/λ/apply/vr") {
        forAll { (t: Term) =>
            assert(vr"a" == Term.Var(NamedDeBruijn("a", 0)))
            assert(!t == Term.Force(t))
            assert(~t == Term.Delay(t))
            assert((t $ t) == Term.Apply(t, t))
            assert(λ("x")(_ => t) == Term.LamAbs("x", t))
            assert(lam("x")(x => t) == Term.LamAbs("x", t))
            assert(lam("x", "y")((_, _) => t) == Term.LamAbs("x", Term.LamAbs("y", t)))
        }
    }

    test("pretty Apply") {
        val t = vr"f" $ vr"x" $ vr"y" $ vr"z"
        assert(t.show == "[f x y z]")
    }
