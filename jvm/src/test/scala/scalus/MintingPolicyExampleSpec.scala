package scalus

import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.Compiler.fieldAsData
import scalus.builtins.Builtins
import scalus.builtins.ByteString
import scalus.builtins.ByteString.given
import scalus.examples.MintingPolicy
import scalus.examples.MintingPolicyV2
import scalus.ledger.api.v1.FromDataInstances.given
import scalus.ledger.api.v1.ToDataInstances.given
import scalus.ledger.api.v1.*
import scalus.ledger.api.v2
import scalus.prelude.List
import scalus.prelude.List.Cons
import scalus.prelude.List.Nil
import scalus.prelude.Maybe.*
import scalus.prelude.Prelude.===
import scalus.prelude.Prelude.given
import scalus.prelude.*
import scalus.sir.SimpleSirToUplcLowering
import scalus.uplc.ArbitraryInstances
import scalus.Compiler.compile
import scalus.uplc.Constant.Pair
import scalus.uplc.Data.FromData
import scalus.uplc.DefaultFun.*
import scalus.uplc.DefaultUni.Bool
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.{_, given}
import scalus.uplc.*
import scalus.utils.Utils

import java.io.ByteArrayInputStream
import scala.collection.immutable
import scala.util.Failure
import scala.util.Success
import scala.util.Try

class MintingPolicyExampleSpec extends BaseValidatorSpec {

  private def scriptContextV1(txInfoInputs: scalus.prelude.List[TxInInfo], value: Value) =
    ScriptContext(
      TxInfo(
        inputs = txInfoInputs,
        outputs = scalus.prelude.List.Nil,
        fee = Value.lovelace(BigInt("188021")),
        mint = value,
        dcert = scalus.prelude.List.Nil,
        withdrawals = scalus.prelude.List.Nil,
        validRange = Interval.always,
        signatories = scalus.prelude.List.Nil,
        data = scalus.prelude.List.Nil,
        id = TxId(hex"1e0612fbd127baddfcd555706de96b46c4d4363ac78c73ab4dee6e6a7bf61fe9")
      ),
      ScriptPurpose.Minting(hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235")
    )

  private def scriptContextV2(txInfoInputs: scalus.prelude.List[v2.TxInInfo], value: Value) =
    v2.ScriptContext(
      v2.TxInfo(
        inputs = txInfoInputs,
        referenceInputs = scalus.prelude.List.Nil,
        outputs = scalus.prelude.List.Nil,
        fee = Value.lovelace(BigInt("188021")),
        mint = value,
        dcert = scalus.prelude.List.Nil,
        withdrawals = AssocMap.empty,
        validRange = Interval.always,
        signatories = scalus.prelude.List.Nil,
        redeemers = AssocMap.empty,
        data = AssocMap.empty,
        id = TxId(hex"1e0612fbd127baddfcd555706de96b46c4d4363ac78c73ab4dee6e6a7bf61fe9")
      ),
      ScriptPurpose.Minting(hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235")
    )

  def withScriptContextV1(
      validator: Term,
      txInfoInputs: scalus.prelude.List[TxInInfo],
      value: Value
  ) =
    import Data.toData
    Program((1, 0, 0), validator $ () $ scriptContextV1(txInfoInputs, value).toData)

  def withScriptContextV2(
      validator: Term,
      txInfoInputs: scalus.prelude.List[TxInInfo],
      value: Value
  ) =
    import Data.toData
    import scalus.ledger.api.v2.ToDataInstances.given
    val txInfoInputsV2 = prelude.List.map(txInfoInputs) { case TxInInfo(txOutRef, txOut) =>
      val txOutV2 =
        v2.TxOut(txOut.address, txOut.value, v2.OutputDatum.NoOutputDatum, Nothing)
      v2.TxInInfo(txOutRef, txOutV2)
    }
    Program((2, 0, 0), validator $ () $ scriptContextV2(txInfoInputsV2, value).toData)

  private def performMintingPolicyValidatorChecks(
      validator: Term
  )(withScriptContext: (Term, scalus.prelude.List[TxInInfo], Value) => Program) = {
    // The minting policy script should succeed when the TxOutRef is spent and the minted tokens are correct
    assertSameResult(Expected.Success(Const(Constant.Unit)))(
      withScriptContext(
        validator,
        List(TxInInfo(hoskyMintTxOutRef, hoskyMintTxOut)),
        Value(
          hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235",
          hex"484f534b59",
          BigInt("1000000000000000")
        )
      )
    )
    // Successfull burn, the minted tokens are negative and TxOutRef is not spent
    assertSameResult(Expected.Success(Const(Constant.Unit)))(
      withScriptContext(
        validator,
        List.empty,
        Value(
          hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235",
          hex"484f534b59",
          BigInt(-100)
        )
      )
    )

    assertSameResult(Expected.Failure("Wrong minted amount"))(
      withScriptContext(
        validator,
        List(TxInInfo(hoskyMintTxOutRef, hoskyMintTxOut)),
        Value(hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235", hex"484f534b59", 2)
      )
    )

    assertSameResult(Expected.Failure("Wrong Policy ID"))(
      withScriptContext(
        validator,
        List(TxInInfo(hoskyMintTxOutRef, hoskyMintTxOut)),
        Value(hex"cc", hex"484f534b59", BigInt("1000000000000000"))
      )
    )

    assertSameResult(Expected.Failure("Wrong Token Name"))(
      withScriptContext(
        validator,
        List(TxInInfo(hoskyMintTxOutRef, hoskyMintTxOut)),
        Value(
          hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235",
          hex"deadbeef",
          BigInt("1000000000000000")
        )
      )
    )

    assertSameResult(Expected.Failure("Burning amount is positive"))(
      withScriptContext(
        validator,
        List.empty,
        Value(
          hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235",
          hex"484f534b59",
          BigInt("1000000000000000")
        )
      )
    )

    assertSameResult(Expected.Failure("Unexpected tokens"))(
      withScriptContext(
        validator,
        List(TxInInfo(hoskyMintTxOutRef, hoskyMintTxOut)),
        AssocMap.singleton(
          hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235",
          AssocMap.fromList(
            List.Cons(
              (hex"484f534b59", BigInt("1000000000000000")),
              List.Cons((hex"deadbeef", BigInt("1000000000000000")), List.Nil)
            )
          )
        )
      )
    )
  }

  val evaledTokens =
    val tokensSIR =
      compile(AssocMap.singleton(ByteString.fromHex("484f534b59"), BigInt("1000000000000000")))
    val tokens = new SimpleSirToUplcLowering().lower(tokensSIR)
    Cek.evalUPLC(tokens)

  test("Minting Policy Validator") {
    val validator = new SimpleSirToUplcLowering(generateErrorTraces = true)
      .lower(MintingPolicy.compiledMintingPolicyScript)
    val appliedValidator =
      validator $ hoskyMintTxOutRef.id.hash $ hoskyMintTxOutRef.idx $ evaledTokens
    val flatSize = ProgramFlatCodec.encodeFlat(Program((1, 0, 0), appliedValidator)).length
    assert(flatSize == 2348)
    performMintingPolicyValidatorChecks(appliedValidator)(withScriptContextV1)
  }

  test("Minting Policy Validator V2") {
    val validator = new SimpleSirToUplcLowering(generateErrorTraces = true)
      .lower(MintingPolicyV2.compiledMintingPolicyScriptV2)
    val appliedValidator =
      validator $ hoskyMintTxOutRef.id.hash $ hoskyMintTxOutRef.idx $ evaledTokens
    val flatSize = ProgramFlatCodec.encodeFlat(Program((2, 0, 0), appliedValidator)).length
    assert(flatSize == 2489)
    performMintingPolicyValidatorChecks(appliedValidator)(withScriptContextV2)
  }

  test("Minting Policy Validator Optimized") {
    // println(MintingPolicy.compiledOptimizedMintingPolicyScript.pretty.render(100))
    val validator = new SimpleSirToUplcLowering(generateErrorTraces = true)
      .lower(MintingPolicy.compiledOptimizedMintingPolicyScript)
    val appliedValidator =
      validator $ hoskyMintTxOutRef.id.hash $ hoskyMintTxOutRef.idx $ evaledTokens
    val flatSize = ProgramFlatCodec.encodeFlat(Program((1, 0, 0), appliedValidator)).length
    assert(flatSize == 1055)
    performMintingPolicyValidatorChecks(appliedValidator)(withScriptContextV1)
  }
}
