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
