package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.{ByteString, Data, given}
import scalus.builtin.ByteString.*
import scalus.builtin.ToDataInstances.given
import scalus.uplc.Constant.Pair
import scalus.uplc.DefaultFun.*
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.{*, given}
import scalus.uplc.eval.*
import scalus.ledger.api.{PlutusLedgerLanguage, ProtocolVersion}

import scala.language.implicitConversions
import scala.reflect.ClassTag

open class CekBuiltinsSpec
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with ArbitraryInstances:

    protected given PlutusVM = PlutusVM.makePlutusV2VM()

    def assertEvalEq(a: Term, b: Term): Unit =
        assert(a.evaluate == b, s"$a != $b")

    def assertEvalThrows[A <: AnyRef: ClassTag](a: Term): Unit =
        assertThrows[A](a.evaluate)

    test("Lazy builtin evaluation") {
        assertEvalEq(AddInteger $ "wrong", Apply(Builtin(AddInteger), "wrong"))
    }

    test("AddInteger") {
        forAll { (a: BigInt, b: BigInt) =>
            assertEvalEq(AddInteger $ a $ b, Const(asConstant(a + b)))
        }

        forAll { (a: Term, b: Term) =>
            (a, b) match
                case (Const(Constant.Integer(aa)), Const(Constant.Integer(bb))) =>
                    val r = aa + bb
                    assertEvalEq(AddInteger $ a $ b, Const(Constant.Integer(r)))
                case _ => assertEvalThrows[Exception](AddInteger $ a $ b)
        }
    }

    test("SubstractInteger") {
        forAll { (a: BigInt, b: BigInt) =>
            assertEvalEq(SubtractInteger $ a $ b, Const(asConstant(a - b)))
        }

        forAll { (a: Term, b: Term) =>
            (a, b) match
                case (Const(Constant.Integer(aa)), Const(Constant.Integer(bb))) =>
                    val r = aa - bb
                    assertEvalEq(SubtractInteger $ a $ b, Const(Constant.Integer(r)))
                case _ => assertEvalThrows[Exception](AddInteger $ a $ b)
        }
    }

    test("DivideInteger") {
        // integer division truncated toward negative infinity
        assertEvalEq(DivideInteger $ 20 $ 3, Const(Constant.Integer(6)))
        assertEvalEq(DivideInteger $ -20 $ -3, Const(Constant.Integer(6)))
        assertEvalEq(DivideInteger $ 20 $ -3, Const(Constant.Integer(-7)))
        assertEvalEq(DivideInteger $ -20 $ 3, Const(Constant.Integer(-7)))
        assertEvalThrows[BuiltinError](DivideInteger $ 1 $ 0)
    }

    test("QuotientInteger") {
        assertEvalEq(QuotientInteger $ -20 $ 3, Const(Constant.Integer(-6)))
        assertEvalThrows[BuiltinError](QuotientInteger $ 20 $ 0)
        forAll { (a: BigInt, b: BigInt) =>
            if b == 0 then assertEvalThrows[BuiltinError](QuotientInteger $ a $ b)
            else assertEvalEq(QuotientInteger $ a $ b, Const(asConstant(a / b)))
        }
    }

    test("RemainderInteger") {
        assertEvalEq(RemainderInteger $ 20 $ 3, Const(Constant.Integer(2)))
        assertEvalEq(RemainderInteger $ -20 $ 3, Const(Constant.Integer(-2)))
        assertEvalEq(RemainderInteger $ 20 $ -3, Const(Constant.Integer(2)))
        assertEvalEq(RemainderInteger $ -20 $ -3, Const(Constant.Integer(-2)))
        assertEvalThrows[BuiltinError](RemainderInteger $ 20 $ 0)
        forAll { (a: BigInt, b: BigInt) =>
            if b == 0 then assertEvalThrows[BuiltinError](RemainderInteger $ a $ b)
            else assertEvalEq(RemainderInteger $ a $ b, Const(asConstant(a % b)))
        }
    }

    test("ModInteger") {
        assertEvalEq(ModInteger $ 20 $ 3, Const(Constant.Integer(2)))
        assertEvalEq(ModInteger $ -20 $ 3, Const(Constant.Integer(1)))
        assertEvalEq(ModInteger $ 20 $ -3, Const(Constant.Integer(-1)))
        assertEvalEq(ModInteger $ -20 $ -3, Const(Constant.Integer(-2)))
        assertEvalThrows[BuiltinError](ModInteger $ 20 $ 0)
    }

    test("(x `quot` y)*y + (x `rem` y) == x") {
        forAll { (x: BigInt, y: BigInt) =>
            whenever(y != 0) {
                val q = QuotientInteger $ x $ y
                val r = RemainderInteger $ x $ y
                val x1 = AddInteger $ (MultiplyInteger $ q $ y) $ r
                assertEvalEq(x1, Const(Constant.Integer(x)))
            }
        }
    }

    test("(x `div` y)*y + (x `mod` y) == x") {
        forAll { (x: BigInt, y: BigInt) =>
            whenever(y != 0) {
                val q = DivideInteger $ x $ y
                val r = ModInteger $ x $ y
                val x1 = AddInteger $ (MultiplyInteger $ q $ y) $ r
                assertEvalEq(x1, Const(Constant.Integer(x)))
            }
        }
    }

    test("EqualsInteger") {
        forAll { (a: BigInt, b: BigInt) =>
            assertEvalEq(EqualsInteger $ a $ b, Const(asConstant(a == b)))
        }
    }

    test("UnConstrData") {
        assert(
          (UnConstrData $ Data.Constr(12, 1 :: Nil)).evaluate ==
              Const(
                Pair(asConstant(12), Constant.List(DefaultUni.Data, List(Constant.Data(Data.I(1)))))
              )
        )
    }

    test("UnMapData") {
        assert(
          (UnMapData $ Data.Map((12, 1) :: Nil)).evaluate ==
              Const(
                Constant.List(
                  DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data),
                  Pair(Constant.Data(12), Constant.Data(1)) :: Nil
                )
              )
        )
    }

    test("UnListData") {
        assert(
          (UnListData $ Data.List(Data.I(12) :: Data.I(1) :: Nil)).evaluate ==
              Const(Constant.List(DefaultUni.Data, Constant.Data(12) :: Constant.Data(1) :: Nil))
        )
    }

    test("UnIData") {
        assert((UnIData $ Data.I(12)).evaluate == Const(Constant.Integer(12)))
    }

    test("UnBData") {
        import scalus.utils.Utils.*
        assert(
          (UnBData $ Data.B(hex"deadbeef")).evaluate == Const(Constant.ByteString(hex"deadbeef"))
        )
    }

    test("ChooseList") {
        // check empty case
        assertEvalEq(
          !(!ChooseList) $ Const(Constant.List(DefaultUni.Integer, Nil)) $ asConstant(
            1
          ) $ asConstant(2),
          Const(asConstant(1))
        )
        // check non-empty case
        assertEvalEq(
          !(!ChooseList) $ Const(
            Constant.List(DefaultUni.Integer, asConstant(333) :: Nil)
          ) $ asConstant(1) $ asConstant(2),
          Const(asConstant(2))
        )

        forAll { (t: Constant) =>
            t match
                case Constant.List(_, v) =>
                    assertEvalEq(
                      !(!ChooseList) $ t $ asConstant(true) $ asConstant(false),
                      Const(Constant.Bool(v.isEmpty))
                    )
                case _ =>
                    assertEvalThrows[Exception](!ChooseList $ t)
        }
    }

    test("NullList") {
        assertEvalEq(
          !NullList $ Const(Constant.List(DefaultUni.Integer, Nil)),
          Const(asConstant(true))
        )

        forAll { (t: Constant) =>
            t match
                case Constant.List(_, v) =>
                    assertEvalEq(!NullList $ t, Const(Constant.Bool(v.isEmpty)))
                case _ =>
                    assertEvalThrows[Exception](!NullList $ t)
        }
    }

    test("MkCons") {
        assertEvalEq(
          !MkCons $ Constant.Integer(0) $ Const(
            Constant.List(DefaultUni.Integer, asConstant(1) :: Nil)
          ),
          Const(Constant.List(DefaultUni.Integer, asConstant(0) :: asConstant(1) :: Nil))
        )
        // should throw if constant types don't match
        assertEvalThrows[Exception](
          !MkCons $ Constant.Unit $ Constant.List(DefaultUni.Integer, Nil)
        )
    }

    test("HeadList") {
        assertEvalEq(
          !HeadList $ Const(Constant.List(DefaultUni.Integer, asConstant(1) :: Nil)),
          Const(asConstant(1))
        )

        forAll { (t: Constant) =>
            t match
                case Constant.List(_, v) if v.nonEmpty =>
                    assertEvalEq(!HeadList $ t, Const(v.head))
                case _ =>
                    assertEvalThrows[Exception](!HeadList $ t)
        }
    }

    test("TailList") {
        assertEvalEq(
          !TailList $ Const(
            Constant.List(DefaultUni.Integer, asConstant(1) :: asConstant(2) :: Nil)
          ),
          Const(Constant.List(DefaultUni.Integer, asConstant(2) :: Nil))
        )

        forAll { (t: Constant) =>
            t match
                case Constant.List(tpe, v) if v.nonEmpty =>
                    assertEvalEq(!TailList $ t, Const(Constant.List(tpe, v.tail)))
                case _ =>
                    assertEvalThrows[Exception](!TailList $ t)
        }
    }

    test("FstPair") {
        assertEvalEq(
          !(!FstPair) $ Const(Constant.Pair(asConstant(1), asConstant(false))),
          Const(asConstant(1))
        )

        /*forAll { (t: Constant) =>
      t match
        case Constant.Pair(a, _) =>
          assertEvalEq(!(!FstPair) $ t, Const(a))
        case _ =>
          assertEvalThrows[Exception](!(!FstPair) $ t)
    }*/
    }

    test("SndPair") {
        assertEvalEq(
          !(!SndPair) $ Const(Constant.Pair(asConstant(1), asConstant(false))),
          Const(asConstant(false))
        )

        forAll { (t: Constant) =>
            t match
                case Constant.Pair(_, b) =>
                    assertEvalEq(!(!SndPair) $ t, Const(b))
                case _ =>
                    assertEvalThrows[Exception](!(!SndPair) $ t)
        }
    }

    test("EqualsByteString") {
        assertEvalEq(
          EqualsByteString $ Const(Constant.ByteString(hex"deadbeef")) $ Const(
            Constant.ByteString(hex"deadbeef")
          ),
          Const(asConstant(true))
        )
        assertEvalEq(
          EqualsByteString $ Const(Constant.ByteString(hex"")) $ Const(
            Constant.ByteString(hex"deadbeef")
          ),
          Const(asConstant(false))
        )

        forAll { (t: Constant) =>
            t match
                case Constant.ByteString(_) =>
                    assertEvalEq(EqualsByteString $ t $ t, Const(asConstant(true)))
                case _ =>
                    assertEvalThrows[Exception](EqualsByteString $ t $ t)
        }
    }

    test("SliceByteString") {
        assertEvalEq(
          SliceByteString $ asConstant(1) $ asConstant(3) $ Const(
            Constant.ByteString(hex"deadbeef")
          ),
          Const(Constant.ByteString(hex"adbeef"))
        )

        assertEvalEq(
          SliceByteString $ asConstant(1) $ asConstant(5) $ Const(
            Constant.ByteString(hex"deadbeef")
          ),
          Const(Constant.ByteString(hex"adbeef"))
        )
    }

    test("verifyEd25519Signature") {
        val sir = compile { scalus.builtin.Builtins.verifyEd25519Signature }
        val verify = sir.toUplc()
        val valid = verify $
            hex"9518c18103cbdab9c6e60b58ecc3e2eb439fef6519bb22570f391327381900a8" $
            ByteString.fromString("hello") $
            hex"f13fa9acffb108114ec060561b58005fb2d69184de0a2d7400b2ea1f111c0794831cc832c92daf4807820dd9458324935e90bec855e8bf076bbbc4e42b727b07"

        assertEvalEq(valid, true)

        val wrongMessage = verify $
            hex"9518c18103cbdab9c6e60b58ecc3e2eb439fef6519bb22570f391327381900a8" $
            ByteString.fromString("NOT hello") $
            hex"f13fa9acffb108114ec060561b58005fb2d69184de0a2d7400b2ea1f111c0794831cc832c92daf4807820dd9458324935e90bec855e8bf076bbbc4e42b727b07"

        assertEvalEq(wrongMessage, false)

        val wrongPubKey = verify $
            hex"AA18c18103cbdab9c6e60b58ecc3e2eb439fef6519bb22570f391327381900a8" $
            ByteString.fromString("hello") $
            hex"f13fa9acffb108114ec060561b58005fb2d69184de0a2d7400b2ea1f111c0794831cc832c92daf4807820dd9458324935e90bec855e8bf076bbbc4e42b727b07"

        assertEvalEq(wrongPubKey, false)

        val wrongSignature = verify $
            hex"9518c18103cbdab9c6e60b58ecc3e2eb439fef6519bb22570f391327381900a8" $
            ByteString.fromString("NOT hello") $
            hex"FF3fa9acffb108114ec060561b58005fb2d69184de0a2d7400b2ea1f111c0794831cc832c92daf4807820dd9458324935e90bec855e8bf076bbbc4e42b727b07"

        assertEvalEq(wrongSignature, false)
    }

    test("verifyEcdsaSecp256k1Signature follows CIP-49") {
        // https://cips.cardano.org/cip/CIP-49
        val sir = compile { scalus.builtin.Builtins.verifyEcdsaSecp256k1Signature }
        val verify = sir.toUplc()
        val pubKey = hex"03427d3132a06e31bf66791dda478b5ebec79bd045247126396fccdf11e42a3627"

        // valid public key, message and signature
        val valid = verify $
            pubKey $
            hex"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824" $
            hex"040f5b6a2bb4e024d47eab02d4073da655af77c0cf0efdb19c6771378da175c45ffac010a1bd9b9a275ad685ea4052f4bc72c0dc27094422ba9379e7bf44b29b"

        assertEvalEq(valid, true)

        // invalid message size
        val invalidMessageSize = verify $
            pubKey $
            hex"deadbeef" $
            hex"040f5b6a2bb4e024d47eab02d4073da655af77c0cf0efdb19c6771378da175c45ffac010a1bd9b9a275ad685ea4052f4bc72c0dc27094422ba9379e7bf44b29b"

        assertEvalThrows[BuiltinError](invalidMessageSize)

        val invalidPubKey = verify $
            hex"FFFF7d3132a06e31bf66791dda478b5ebec79bd045247126396fccdf11e42a3627" $
            hex"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824" $
            hex"040f5b6a2bb4e024d47eab02d4073da655af77c0cf0efdb19c6771378da175c45ffac010a1bd9b9a275ad685ea4052f4bc72c0dc27094422ba9379e7bf44b29b"

        assertEvalThrows[BuiltinError](invalidPubKey)

        val invalidSignature = verify $
            pubKey $
            hex"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824" $
            hex"deadbeef"

        assertEvalThrows[BuiltinError](invalidSignature)

        val wrongSignature = verify $
            pubKey $
            hex"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824" $
            hex"FF0f5b6a2bb4e024d47eab02d4073da655af77c0cf0efdb19c6771378da175c45ffac010a1bd9b9a275ad685ea4052f4bc72c0dc27094422ba9379e7bf44b29b"

        assertEvalEq(wrongSignature, false)
    }

    test("verifySchnorrSecp256k1Signature follows CIP-49") {
        // https://cips.cardano.org/cip/CIP-49
        val sir = compile { scalus.builtin.Builtins.verifySchnorrSecp256k1Signature }
        val verify = sir.toUplc()
        val pubKey = hex"427d3132a06e31bf66791dda478b5ebec79bd045247126396fccdf11e42a3627"

        // valid public key, message and signature
        val valid = verify $
            pubKey $
            hex"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824" $
            hex"4fd97a0c4ad719f89cba68a522e0dee13bcf656ae9c0a395404cda858a7992d8dea979dbc4c83659d695b7d380fe8a75264ba51a63a53fc2a8bd225e50f223f4"

        assertEvalEq(valid, true)

        val invalidPubKey = verify $
            hex"FFFF7d3132a06e31bf66791dda478b5ebec79bd045247126396fccdf11e42a3627" $
            hex"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824" $
            hex"4fd97a0c4ad719f89cba68a522e0dee13bcf656ae9c0a395404cda858a7992d8dea979dbc4c83659d695b7d380fe8a75264ba51a63a53fc2a8bd225e50f223f4"

        assertEvalThrows[BuiltinError](invalidPubKey)

        val invalidSignature = verify $
            pubKey $
            hex"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824" $
            hex"deadbeef"

        assertEvalThrows[BuiltinError](invalidSignature)

        val wrongSignature = verify $
            pubKey $
            hex"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824" $
            hex"FFd97a0c4ad719f89cba68a522e0dee13bcf656ae9c0a395404cda858a7992d8dea979dbc4c83659d695b7d380fe8a75264ba51a63a53fc2a8bd225e50f223f4"

        assertEvalEq(wrongSignature, false)
    }

    test("AndByteString") {
        given PlutusVM = PlutusVM.makePlutusV3VM(
          MachineParams.defaultParamsFor(PlutusLedgerLanguage.PlutusV3, ProtocolVersion.conwayPV)
        )

//        assert(andByteString(true, hex"00", hex"ff") == hex"00")
        assertEvalEq(AndByteString $ true $ hex"00" $ hex"ff", hex"00")
    }
