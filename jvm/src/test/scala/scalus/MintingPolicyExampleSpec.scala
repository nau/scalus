package scalus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.uplc.ArbitraryInstances
import scalus.builtins.ByteString.given
import scalus.ledger.api.v1.*
import scalus.ledger.api.v1.Instances.given
import scalus.uplc.Constant.Pair
import scalus.uplc.DefaultFun.*
import scalus.uplc.Data.FromData
import scalus.uplc.DefaultUni.{asConstant, Bool}
import scalus.Compiler.{compile, fieldAsData}
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.{*, given}
import scalus.uplc.*
import scala.collection.immutable
import scalus.builtins.Builtins
import scalus.builtins.ByteString
import scalus.examples.MintingPolicy
import scalus.Prelude.Maybe.*
import scalus.Prelude.List
import scalus.Prelude.*
import scalus.Prelude.{===, given}
import scalus.Prelude.List.{Cons, Nil}
import scalus.sir.SimpleSirToUplcLowering
import scalus.utils.Utils
import java.io.ByteArrayInputStream
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import io.bullet.borer.Cbor

class MintingPolicyExampleSpec extends BaseValidatorSpec {

  private def scriptContext(txInfoInputs: scalus.Prelude.List[TxInInfo], value: Value) =
    ScriptContext(
      TxInfo(
        txInfoInputs = txInfoInputs,
        txInfoOutputs = scalus.Prelude.List.Nil,
        txInfoFee = Value.lovelace(BigInt("188021")),
        txInfoMint = value,
        txInfoDCert = scalus.Prelude.List.Nil,
        txInfoWdrl = scalus.Prelude.List.Nil,
        txInfoValidRange = Interval.always,
        txInfoSignatories = scalus.Prelude.List.Nil,
        txInfoData = scalus.Prelude.List.Nil,
        txInfoId = TxId(hex"1e0612fbd127baddfcd555706de96b46c4d4363ac78c73ab4dee6e6a7bf61fe9")
      ),
      ScriptPurpose.Minting(hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235")
    )

  private def performMintingPolicyValidatorChecks(validator: Term) = {
    import Data.toData
    def appliedScript(ctx: ScriptContext) = Program((1, 0, 0), validator $ () $ ctx.toData)

    def withScriptContext(txInfoInputs: scalus.Prelude.List[TxInInfo], value: Value) =
      appliedScript(scriptContext(txInfoInputs, value))

    // The minting policy script should succeed when the TxOutRef is spent and the minted tokens are correct
    assertSameResult(Expected.Success(Const(Constant.Unit)))(
      withScriptContext(
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
        List(TxInInfo(hoskyMintTxOutRef, hoskyMintTxOut)),
        Value(hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235", hex"484f534b59", 2)
      )
    )

    assertSameResult(Expected.Failure("Wrong Policy ID"))(
      withScriptContext(
        List(TxInInfo(hoskyMintTxOutRef, hoskyMintTxOut)),
        Value(hex"cc", hex"484f534b59", BigInt("1000000000000000"))
      )
    )

    assertSameResult(Expected.Failure("Wrong Token Name"))(
      withScriptContext(
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
      validator $ hoskyMintTxOutRef.txOutRefId.hash $ hoskyMintTxOutRef.txOutRefIdx $ evaledTokens
    val flatSize = ProgramFlatCodec.encodeFlat(Program((1, 0, 0), appliedValidator)).length
    assert(flatSize == 2385)
    performMintingPolicyValidatorChecks(appliedValidator)
  }

  test("Minting Policy Validator Optimized") {
    val validator = new SimpleSirToUplcLowering(generateErrorTraces = true)
      .lower(MintingPolicy.compiledOptimizedMintingPolicyScript)
    val appliedValidator =
      validator $ hoskyMintTxOutRef.txOutRefId.hash $ hoskyMintTxOutRef.txOutRefIdx $ evaledTokens
    val flatSize = ProgramFlatCodec.encodeFlat(Program((1, 0, 0), appliedValidator)).length
    assert(flatSize == 1058)
    performMintingPolicyValidatorChecks(appliedValidator)
  }
}
