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

class CekJVMSpec extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances:
  def runUPLC(code: String) = {
    import scala.sys.process.*
    val cmd = "/Users/nau/projects/scalus/uplc evaluate"
    val out = cmd.#<(new ByteArrayInputStream(code.getBytes("UTF-8"))).!!
    println(out)
  }

  def evalUPLC(code: String): Term = {
    import scala.sys.process.*
    val cmd = "/Users/nau/projects/scalus/uplc evaluate"
    val out = cmd.#<(new ByteArrayInputStream(code.getBytes("UTF-8"))).!!
//    println(out)
    UplcParser.term
      .parse(out)
      .map(_._2)
      .getOrElse(
        throw new Exception(
          s"Could not parse: $out"
        )
      )
  }

  def run(code: String) = {
    val parser = UplcParser
    for
      program <- parser.parseProgram(code)
      evaled = Cek.evalUPLCProgram(program)
    do println(evaled.pretty.render(80))
  }

  def eval(code: String): Term = {
    val parser = UplcParser
    parser.parseProgram(code).map(Cek.evalUPLCProgram).getOrElse(sys.error("Parse error"))
  }

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
    def check(code: String, result: Boolean) =
      assert(evalUPLC(code) == Const(asConstant(result)))
      assert(eval(code) == Const(asConstant(result)))

    check("(program 1.0.0 [[(builtin equalsInteger) (con integer 0)] (con integer 0)])", true)
    check("(program 1.0.0 [[(builtin equalsInteger) (con integer 1)] (con integer 1)])", true)
    check(
      "(program 1.0.0 [[(builtin equalsInteger) (con integer -1234567890)] (con integer -1234567890)])",
      true
    )
    check("(program 1.0.0 [[(builtin equalsInteger) (con integer 1)] (con integer 2)])", false)
    {
      val code = "(program 1.0.0 [[(builtin equalsInteger) (con bool True)] (con integer 2)])"
      assertThrows[Exception] { evalUPLC(code) }
      assertThrows[Exception] { eval(code) }
    }

    {
      val code = "(program 1.0.0 [[(builtin equalsInteger) (con integer 1)] (con bool True)])"
      assertThrows[Exception](evalUPLC(code))
      assertThrows[Exception](eval(code))
    }

    forAll { (a: BigInt, b: BigInt) =>
      Cek.evalUPLC(EqualsInteger $ a $ a) match
        case Const(Constant.Bool(true)) => assert(true)
        case r                          => fail(s"Expected true but got ${r.pretty.render(80)}")

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

  // Apparently plutus-conformance doesn't exist on the Plutus V2 commit we're using
  ignore("conformance") {
    def check(name: String) =
      val path =
        s"/Users/nau/projects/iohk/plutus/plutus-conformance/test-cases/uplc/evaluation"
      val code = fromFile(s"$path/$name.uplc").mkString
      val expected = fromFile(s"$path/$name.uplc.expected").mkString
      println(eval(code).pretty.render(80))
      assert(eval(code) == eval(expected))

    check("builtin/addInteger/addInteger")
    check("builtin/addInteger-uncurried/addInteger-uncurried")
    check("builtin/equalsInteger/equalsInteger")
    check("builtin/ifThenElse/ifThenElse1/ifThenElse1")

    // Examples
    check("example/factorial/factorial")
    check("example/fibonacci/fibonacci")
  }

  test("simple validator example") {
    import TermDSL.*
    import scalus.utils.Utils.*

    // simple validator that checks that the spending transaction has no outputs
    // it's a gift to the validators community
    val validator = λ("redeemer", "datum", "ctx") {
      // ScriptContext{scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose }
      val scriptContext = DefaultFun.UnConstrData $ Var(NamedDeBruijn("ctx"))
      // ScriptContext args
      val ctxArgs = !(!DefaultFun.SndPair) $ scriptContext
      // second in the list
      val txInfo = DefaultFun.UnConstrData $ (!DefaultFun.HeadList $ ctxArgs)
      val txInfoArgs = !(!DefaultFun.SndPair) $ txInfo
      val txInfoOutputs =
        !DefaultFun.HeadList $ (!DefaultFun.TailList $ txInfoArgs)
      val isTxInfoOutputsEmpty = !DefaultFun.NullList $ (DefaultFun.UnListData $ txInfoOutputs)
      val result = !(!DefaultFun.IfThenElse $ isTxInfoOutputsEmpty $ ~() $ ~Error("Tx has outputs"))
      result
    }
    assert(validator == Example.giftValidator.term)

    val program = Program((1, 0, 0), validator).pretty.render(80)

    val bytes = ExprBuilder.uplcToFlat(program)
//    println(s"${bytes.length} bytes: ${bytesToHex(bytes)}")
    assert(bytes.length == 34)

    import Data.*

    val scriptContext =
      ScriptContext(
        TxInfo(
          Nil,
          Nil,
          Value.zero,
          Value.zero,
          Nil,
          Nil,
          Interval.always,
          Nil,
          Nil,
          TxId(hex"bb")
        ),
        ScriptPurpose.Spending(TxOutRef(TxId(hex"deadbeef"), 0))
      )
    val appliedScript = Program((1, 0, 0), validator $ () $ () $ scriptContext.toData)
    assert(Cek.evalUPLCProgram(appliedScript) == Const(asConstant(())))
  }

  test("PubKey Validator example") {
    import scalus.ledger.api.v1.*
    import scalus.utils.Utils.*

    // simple validator that checks that the spending transaction
    // has a signature of the given public key hash
    val validator = Example.pubKeyValidator(PubKeyHash(hex"deadbeef"))

    println(validator.term.pretty.render(80))

    import Data.*

    def scriptContext(sigs: immutable.List[PubKeyHash]) =
      ScriptContext(
        TxInfo(
          Nil,
          Nil,
          Value.zero,
          Value.zero,
          Nil,
          Nil,
          Interval.always,
          sigs,
          Nil,
          TxId(hex"bb")
        ),
        ScriptPurpose.Spending(TxOutRef(TxId(hex"aa"), 0))
      )
    def appliedScript(ctx: ScriptContext) =
      Program((1, 0, 0), validator.term $ () $ () $ ctx.toData)

    assert(
      Cek.evalUPLCProgram(
        appliedScript(scriptContext(PubKeyHash(hex"000000") :: PubKeyHash(hex"deadbeef") :: Nil))
      ) == Const(
        asConstant(())
      )
    )

    assertThrows[EvaluationFailure](
      Cek.evalUPLCProgram(appliedScript(scriptContext(PubKeyHash(hex"000000") :: Nil))) == Const(
        asConstant(())
      )
    )

    assertThrows[EvaluationFailure](
      Cek.evalUPLCProgram(appliedScript(scriptContext(Nil))) == Const(
        asConstant(())
      )
    )

    val flatValidator = ExprBuilder.uplcToFlat(Program((1, 0, 0), validator.term).pretty.render(80))
    assert(flatValidator.length == 95)
  }

  test("Minging Policy example") {
    import scalus.ledger.api.v1.*
    import scalus.utils.Utils.*

    // simple validator that checks that the spending transaction
    // has a signature of the given public key hash

    val txOutRef = TxOutRef(TxId(hex"aa"), 1)
    val fakeTxOut = TxOut(
      txOutAddress = Address(Credential.PubKeyCredential(PubKeyHash(hex"deadbeef")), None),
      Value.zero,
      None
    )
    val validator = Example.mintingPolicyScript(txOutRef, hex"deadbeef")

    println(validator.term.pretty.render(80))

    import Data.*

    def scriptContext(txInfoInputs: immutable.List[TxInInfo], value: Value) =
      ScriptContext(
        TxInfo(
          txInfoInputs = txInfoInputs,
          txInfoOutputs = Nil,
          txInfoFee = Value.zero,
          txInfoMint = value,
          txInfoDCert = Nil,
          txInfoWdrl = Nil,
          txInfoValidRange = Interval.always,
          txInfoSignatories = Nil,
          txInfoData = Nil,
          txInfoId = TxId(hex"bb")
        ),
        ScriptPurpose.Minting(hex"ca")
      )

    def appliedScript(ctx: ScriptContext) =
      println(ctx.toData)
      Program((1, 0, 0), validator.term $ () $ ctx.toData)

    assert(
      Cek.evalUPLCProgram(
        appliedScript(
          scriptContext(TxInInfo(txOutRef, fakeTxOut) :: Nil, Value(hex"ca", hex"deadbeef", 1))
        )
      ) == Const(
        asConstant(())
      )
    )

    // TODO - add more tests

    assertThrows[EvaluationFailure](
      Cek.evalUPLCProgram(
        appliedScript(
          scriptContext(TxInInfo(txOutRef, fakeTxOut) :: Nil, Value(hex"ca", hex"deadbeef", 2))
        )
      )
    )

    assertThrows[BuiltinError](
      Cek.evalUPLCProgram(
        appliedScript(
          scriptContext(TxInInfo(txOutRef, fakeTxOut) :: Nil, Value(hex"cc", hex"deadbeef", 1))
        )
      )
    )

    val flatValidator = ExprBuilder.uplcToFlat(Program((1, 0, 0), validator.term).pretty.render(80))
    assert(flatValidator.length == 286)
  }

  ignore("fieldAsData macro test") {
    import Data.*
    import scalus.ledger.api.v1.*
    import scalus.utils.Utils.*

    val txInfo = TxInfo(
      Nil,
      Nil,
      Value.zero,
      Value.zero,
      Nil,
      Nil,
      Interval.always,
      Nil,
      Nil,
      TxId(hex"bb")
    )
    import ExprBuilder.*
    val fee = unIData(fieldAsData[TxInfo](_.txInfoFee).apply(Expr(txInfo.toData)))
    val txId = unBData(fieldAsData[TxInfo](_.txInfoId).apply(Expr(txInfo.toData)))
    assert(Cek.evalUPLC(fee.term) == Const(asConstant(BigInt(123))))
    assert(Cek.evalUPLC(txId.term) == Const(asConstant(hex"bb")))
    println(txId)
    println(Cek.evalUPLC(txId.term))

  }

  test("field macro test") {
    import Data.*
    import scalus.ledger.api.v1.*
    import scalus.ledger.api.v1.Instances.given
    import scalus.utils.Utils.*

    val txOutRef = TxOutRef(TxId(hex"bb"), 123)
    import ExprBuilder.*
    val applied = field[TxOutRef](_.txOutRefIdx).apply(Expr(txOutRef.toData))
    assert(Cek.evalUPLC(applied.term) == Const(asConstant(BigInt(123))))
    val applied2 = field[TxOutRef](_.txOutRefId).apply(Expr(txOutRef.toData))
    val aaa = unConstrData(applied2)
    assert(Cek.evalUPLC(applied2.term) == Const(asConstant(TxId(hex"bb").toData)))

  }
