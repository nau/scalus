package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.ledger.api.v1.*
import scalus.uplc.Constant.Pair
import scalus.uplc.DefaultFun.*
import scalus.uplc.DefaultUni.{Bool, ByteString, asConstant}
import scalus.uplc.ExprBuilder.{sndPair, unConstrData}
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.{*, given}
import scalus.utils.Utils

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.math.RoundingMode
import scala.collection.immutable
import scala.io.Source.fromFile

class CekBuiltinsSpec extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances:

  test("AddInteger") {
    forAll { (a: BigInt, b: BigInt) =>
      Cek.evalUPLC(AddInteger $ a $ b) match
        case Const(Constant.Integer(r)) => assert(r == (a + b))
        case r                          => fail(s"Expected true but got ${r.pretty.render(80)}")
    }

    forAll { (a: Term, b: Term) =>
      (a, b) match
        case (Const(Constant.Integer(aa)), Const(Constant.Integer(bb))) =>
          val r = aa + bb
          assert(Cek.evalUPLC(AddInteger $ a $ b) == Const(Constant.Integer(r)))
        case _ => assertThrows[Exception](Cek.evalUPLC(AddInteger $ a $ b))
    }
  }

  test("SubstractInteger") {
    forAll { (a: BigInt, b: BigInt) =>
      Cek.evalUPLC(SubtractInteger $ a $ b) match
        case Const(Constant.Integer(r)) => assert(r == (a - b))
        case r                          => fail(s"Expected true but got ${r.pretty.render(80)}")
    }

    forAll { (a: Term, b: Term) =>
      (a, b) match
        case (Const(Constant.Integer(aa)), Const(Constant.Integer(bb))) =>
          val r = aa - bb
          assert(Cek.evalUPLC(SubtractInteger $ a $ b) == Const(Constant.Integer(r)))
        case _ => assertThrows[Exception](Cek.evalUPLC(AddInteger $ a $ b))
    }
  }

  test("DivideInteger") {
    // integer division truncated toward negative infinity
    assert(Cek.evalUPLC(DivideInteger $ 20 $ 3) == Const(Constant.Integer(6)))
    assert(Cek.evalUPLC(DivideInteger $ -20 $ -3) == Const(Constant.Integer(6)))
    assert(Cek.evalUPLC(DivideInteger $ 20 $ -3) == Const(Constant.Integer(-7)))
    assert(Cek.evalUPLC(DivideInteger $ -20 $ 3) == Const(Constant.Integer(-7)))
    assertThrows[BuiltinError](Cek.evalUPLC(DivideInteger $ 1 $ 0))
  }

  test("QuotientInteger") {
    assert(Cek.evalUPLC(QuotientInteger $ -20 $ 3) == Const(Constant.Integer(-6)))
    assertThrows[BuiltinError](Cek.evalUPLC(QuotientInteger $ 20 $ 0))
    forAll { (a: BigInt, b: BigInt) =>
      if b == 0 then assertThrows[BuiltinError](Cek.evalUPLC(QuotientInteger $ a $ b))
      else
        Cek.evalUPLC(QuotientInteger $ a $ b) match
          case Const(Constant.Integer(r)) => assert(r == (a / b))
          case r                          => fail(s"Expected true but got ${r.pretty.render(80)}")
    }
  }

  test("RemainderInteger") {
    assert(Cek.evalUPLC(RemainderInteger $ 20 $ 3) == Const(Constant.Integer(2)))
    assert(Cek.evalUPLC(RemainderInteger $ -20 $ 3) == Const(Constant.Integer(-2)))
    assert(Cek.evalUPLC(RemainderInteger $ 20 $ -3) == Const(Constant.Integer(2)))
    assert(Cek.evalUPLC(RemainderInteger $ -20 $ -3) == Const(Constant.Integer(-2)))
    assertThrows[BuiltinError](Cek.evalUPLC(RemainderInteger $ 20 $ 0))
    forAll { (a: BigInt, b: BigInt) =>
      if b == 0 then assertThrows[BuiltinError](Cek.evalUPLC(RemainderInteger $ a $ b))
      else
        Cek.evalUPLC(RemainderInteger $ a $ b) match
          case Const(Constant.Integer(r)) => assert(r == (a % b))
          case r                          => fail(s"Expected true but got ${r.pretty.render(80)}")
    }
  }

  test("ModInteger") {
    assert(Cek.evalUPLC(ModInteger $ 20 $ 3) == Const(Constant.Integer(2)))
    assert(Cek.evalUPLC(ModInteger $ -20 $ 3) == Const(Constant.Integer(1)))
    assert(Cek.evalUPLC(ModInteger $ 20 $ -3) == Const(Constant.Integer(-1)))
    assert(Cek.evalUPLC(ModInteger $ -20 $ -3) == Const(Constant.Integer(-2)))
    assertThrows[BuiltinError](Cek.evalUPLC(ModInteger $ 20 $ 0))
  }

  test("(x `quot` y)*y + (x `rem` y) == x") {
    forAll { (x: BigInt, y: BigInt) =>
      whenever(y != 0) {
        val q = Cek.evalUPLC(QuotientInteger $ x $ y)
        val r = Cek.evalUPLC(RemainderInteger $ x $ y)
        val x1 = Cek.evalUPLC(AddInteger $ (MultiplyInteger $ q $ y) $ r)
        assert(x1 == Const(Constant.Integer(x)))
      }
    }
  }

  test("(x `div` y)*y + (x `mod` y) == x") {
    forAll { (x: BigInt, y: BigInt) =>
      whenever(y != 0) {
        val q = Cek.evalUPLC(DivideInteger $ x $ y)
        val r = Cek.evalUPLC(ModInteger $ x $ y)
        val x1 = Cek.evalUPLC(AddInteger $ (MultiplyInteger $ q $ y) $ r)
        assert(x1 == Const(Constant.Integer(x)))
      }
    }
  }

  test("EqualsInteger") {
    forAll { (a: BigInt, b: BigInt) =>
      Cek.evalUPLC(EqualsInteger $ a $ b) match
        case Const(Constant.Bool(r)) => assert(r == (a == b))
        case r                       => fail(s"Expected true but got ${r.pretty.render(80)}")
    }
  }

  test("UnConstrData") {
    assert(
      Cek.evalUPLC(DefaultFun.UnConstrData $ Data.Constr(12, 1 :: Nil)) == Const(
        Pair(asConstant(12), Constant.List(DefaultUni.Data, List(Constant.Data(Data.I(1)))))
      )
    )

    forAll { (t: Data) =>
      t match
        case Data.Constr(constr, args) =>
          val result = Cek.evalUPLC(DefaultFun.UnConstrData $ t)
          assert(
            result == Const(
              Pair(asConstant(constr), Constant.List(DefaultUni.Data, args.map(asConstant)))
            )
          )
        case _ =>
          assertThrows[Exception](Cek.evalUPLC(DefaultFun.UnConstrData $ t))
    }

    // FIXME: now Arbitrary[Term] doesn't generate Data. When it does, update this test
    forAll { (t: Term) =>
      assertThrows[Exception](
        Cek.evalUPLC(DefaultFun.UnConstrData $ t)
      )
    }
  }

  test("UnMapData") {
    assert(
      Cek.evalUPLC(DefaultFun.UnMapData $ Data.Map((12, 1) :: Nil)) == Const(
        Constant.List(
          DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data),
          Pair(Constant.Data(12), Constant.Data(1)) :: Nil
        )
      )
    )

    forAll { (t: Data) =>
      t match
        case Data.Map(elems) =>
          val result = Cek.evalUPLC(DefaultFun.UnMapData $ t)
          assert(
            result == Const(
              Constant.List(
                DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data),
                elems.map { case (k, v) =>
                  Pair(Constant.Data(k), Constant.Data(v))
                }
              )
            )
          )
        case _ =>
          assertThrows[Exception](Cek.evalUPLC(DefaultFun.UnMapData $ t))
    }

    // FIXME: now Arbitrary[Term] doesn't generate Data. When it does, update this test
    forAll { (t: Term) =>
      assertThrows[Exception](
        Cek.evalUPLC(DefaultFun.UnMapData $ t)
      )
    }
  }

  test("UnListData") {
    assert(
      Cek.evalUPLC(DefaultFun.UnListData $ Data.List(Data.I(12) :: Data.I(1) :: Nil)) == Const(
        Constant.List(
          DefaultUni.Data,
          Constant.Data(12) :: Constant.Data(1) :: Nil
        )
      )
    )

    forAll { (t: Data) =>
      t match
        case Data.List(elems) =>
          val result = Cek.evalUPLC(DefaultFun.UnListData $ t)
          assert(
            result == Const(
              Constant.List(
                DefaultUni.Data,
                elems.map(Constant.Data.apply)
              )
            )
          )
        case _ =>
          assertThrows[Exception](Cek.evalUPLC(DefaultFun.UnListData $ t))
    }

    // FIXME: now Arbitrary[Term] doesn't generate Data. When it does, update this test
    forAll { (t: Term) =>
      assertThrows[Exception](
        Cek.evalUPLC(DefaultFun.UnListData $ t)
      )
    }
  }

  test("UnIData") {
    assert(Cek.evalUPLC(DefaultFun.UnIData $ Data.I(12)) == Const(Constant.Integer(12)))

    forAll { (t: Data) =>
      t match
        case Data.I(v) =>
          val result = Cek.evalUPLC(DefaultFun.UnIData $ t)
          assert(result == Const(Constant.Integer(v)))
        case _ =>
          assertThrows[Exception](Cek.evalUPLC(DefaultFun.UnIData $ t))
    }

    // FIXME: now Arbitrary[Term] doesn't generate Data. When it does, update this test
    forAll { (t: Term) =>
      assertThrows[Exception](
        Cek.evalUPLC(DefaultFun.UnIData $ t)
      )
    }
  }

  test("UnBData") {
    import scalus.utils.Utils.*
    assert(
      Cek.evalUPLC(DefaultFun.UnBData $ Data.B(hex"deadbeef")) == Const(
        Constant.ByteString(hex"deadbeef")
      )
    )

    forAll { (t: Data) =>
      t match
        case Data.B(v) =>
          val result = Cek.evalUPLC(DefaultFun.UnBData $ t)
          assert(result == Const(Constant.ByteString(v)))
        case _ =>
          assertThrows[Exception](Cek.evalUPLC(DefaultFun.UnBData $ t))
    }

    // FIXME: now Arbitrary[Term] doesn't generate Data. When it does, update this test
    forAll { (t: Term) =>
      assertThrows[Exception](
        Cek.evalUPLC(DefaultFun.UnBData $ t)
      )
    }
  }

  test("ChooseList") {
    import scalus.utils.Utils.*
    // check empty case
    assert(
      Cek.evalUPLC(
        !(!DefaultFun.ChooseList) $ Const(Constant.List(DefaultUni.Integer, Nil)) $
          asConstant(1) $
          asConstant(2)
      ) == Const(
        asConstant(1)
      )
    )
    // check non-empty case
    assert(
      Cek.evalUPLC(
        !(!DefaultFun.ChooseList) $ Const(
          Constant.List(DefaultUni.Integer, asConstant(333) :: Nil)
        ) $ asConstant(1) $ asConstant(2)
      ) == Const(
        asConstant(2)
      )
    )

    forAll { (t: Constant) =>
      t match
        case Constant.List(_, v) =>
          val result =
            Cek.evalUPLC(!(!DefaultFun.ChooseList) $ t $ asConstant(true) $ asConstant(false))
          assert(result == Const(Constant.Bool(v.isEmpty)))
        case _ =>
          assertThrows[Exception](Cek.evalUPLC(!DefaultFun.ChooseList $ t))
    }
  }

  test("NullList") {
    import scalus.utils.Utils.*
    assert(
      Cek.evalUPLC(!DefaultFun.NullList $ Const(Constant.List(DefaultUni.Integer, Nil))) == Const(
        asConstant(true)
      )
    )

    forAll { (t: Constant) =>
      t match
        case Constant.List(_, v) =>
          val result = Cek.evalUPLC(!DefaultFun.NullList $ t)
          assert(result == Const(Constant.Bool(v.isEmpty)))
        case _ =>
          assertThrows[Exception](Cek.evalUPLC(!DefaultFun.NullList $ t))
    }
  }

  test("HeadList") {
    import scalus.utils.Utils.*
    assert(
      Cek.evalUPLC(
        !DefaultFun.HeadList $ Const(Constant.List(DefaultUni.Integer, asConstant(1) :: Nil))
      ) == Const(
        asConstant(1)
      )
    )

    forAll { (t: Constant) =>
      t match
        case Constant.List(_, v) if v.nonEmpty =>
          val result = Cek.evalUPLC(!DefaultFun.HeadList $ t)
          assert(result == Const(v.head))
        case _ =>
          assertThrows[Exception](Cek.evalUPLC(!DefaultFun.HeadList $ t))
    }
  }

  test("TailList") {
    import scalus.utils.Utils.*
    assert(
      Cek.evalUPLC(
        !DefaultFun.TailList $ Const(
          Constant.List(DefaultUni.Integer, asConstant(1) :: asConstant(2) :: Nil)
        )
      ) == Const(
        Constant.List(DefaultUni.Integer, asConstant(2) :: Nil)
      )
    )

    forAll { (t: Constant) =>
      t match
        case Constant.List(tpe, v) if v.nonEmpty =>
          val result = Cek.evalUPLC(!DefaultFun.TailList $ t)
          assert(result == Const(Constant.List(tpe, v.tail)))
        case _ =>
          assertThrows[Exception](Cek.evalUPLC(!DefaultFun.TailList $ t))
    }
  }

  test("FstPair") {
    import scalus.utils.Utils.*
    assert(
      Cek.evalUPLC(
        !(!DefaultFun.FstPair) $ Const(Constant.Pair(asConstant(1), asConstant(false)))
      ) == Const(
        asConstant(1)
      )
    )

    forAll { (t: Constant) =>
      t match
        case Constant.Pair(a, _) =>
          val result = Cek.evalUPLC(!(!DefaultFun.FstPair) $ t)
          assert(result == Const(a))
        case _ =>
          assertThrows[Exception](Cek.evalUPLC(!(!DefaultFun.FstPair) $ t))
    }
  }

  test("SndPair") {
    import scalus.utils.Utils.*
    assert(
      Cek.evalUPLC(
        !(!DefaultFun.SndPair) $ Const(Constant.Pair(asConstant(1), asConstant(false)))
      ) == Const(
        asConstant(false)
      )
    )

    forAll { (t: Constant) =>
      t match
        case Constant.Pair(_, b) =>
          val result = Cek.evalUPLC(!(!DefaultFun.SndPair) $ t)
          assert(result == Const(b))
        case _ =>
          assertThrows[Exception](Cek.evalUPLC(!(!DefaultFun.SndPair) $ t))
    }
  }

  test("EqualsByteString") {
    import scalus.utils.Utils.*
    assert(
      Cek.evalUPLC(
        DefaultFun.EqualsByteString $ Const(Constant.ByteString(hex"deadbeef")) $ Const(
          Constant.ByteString(hex"deadbeef")
        )
      ) == Const(
        asConstant(true)
      )
    )
    assert(
      Cek.evalUPLC(
        DefaultFun.EqualsByteString $ Const(Constant.ByteString(hex"")) $ Const(
          Constant.ByteString(hex"deadbeef")
        )
      ) == Const(
        asConstant(false)
      )
    )

    forAll { (t: Constant) =>
      t match
        case Constant.ByteString(_) =>
          val result = Cek.evalUPLC(DefaultFun.EqualsByteString $ t $ t)
          assert(result == Const(asConstant(true)))
        case _ =>
          assertThrows[Exception](Cek.evalUPLC(DefaultFun.EqualsByteString $ t $ t))
    }
  }
