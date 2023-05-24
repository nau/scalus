package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.builtins.ByteString.given
import scalus.ledger.api.v1.*
import scalus.uplc.Constant.Pair
import scalus.uplc.DefaultFun.*
import scalus.uplc.DefaultUni.{asConstant, Bool, ByteString}
import scalus.uplc.ExprBuilder.{sndPair, unConstrData}
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.{*, given}
import scalus.prelude.List.{Cons, Nil}
import scalus.prelude.Maybe.*

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.math.RoundingMode
import scala.collection.immutable
import scala.io.Source.fromFile
import scalus.utils.Utils

class CekJVMSpec extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances:
  def evalUPLC(code: String): Term = {
    val out = Utils.uplcEvaluate(code)
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
  test("conformance") {
    def check(name: String) =
      val path =
        s"../plutus-conformance/test-cases/uplc/evaluation"
      val code = fromFile(s"$path/$name.uplc").mkString
      val expected = fromFile(s"$path/$name.uplc.expected").mkString
      // println(eval(code).pretty.render(80))
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

    val bytes = Utils.uplcToFlat(program)
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

//    println(validator.term.pretty.render(80))

    import Data.*

    def scriptContext(sigs: scalus.prelude.List[PubKeyHash]) =
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
        appliedScript(
          scriptContext(Cons(PubKeyHash(hex"000000"), Cons(PubKeyHash(hex"deadbeef"), Nil)))
        )
      ) == Const(
        asConstant(())
      )
    )

    assertThrows[EvaluationFailure](
      Cek.evalUPLCProgram(
        appliedScript(scriptContext(Cons(PubKeyHash(hex"000000"), Nil)))
      ) == Const(
        asConstant(())
      )
    )

    assertThrows[EvaluationFailure](
      Cek.evalUPLCProgram(appliedScript(scriptContext(Nil))) == Const(
        asConstant(())
      )
    )

    val flatValidator = Utils.uplcToFlat(Program((1, 0, 0), validator.term).pretty.render(80))
    assert(flatValidator.length == 95)
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
    val fee = unIData(fieldAsData[TxInfo](_.fee).apply(Expr(txInfo.toData)))
    val txId = unBData(fieldAsData[TxInfo](_.id).apply(Expr(txInfo.toData)))
    assert(Cek.evalUPLC(fee.term) == Const(asConstant(BigInt(123))))
    assert(Cek.evalUPLC(txId.term) == Const(asConstant(hex"bb")))
    println(txId)
    println(Cek.evalUPLC(txId.term))

  }
