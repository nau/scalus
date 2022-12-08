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
import scalus.Predef.Maybe.*
import scalus.Predef.List
import scalus.Predef.*
import scalus.Predef.List.{Cons, Nil}
import scalus.sir.SimpleSirToUplcLowering
import scalus.utils.Utils
import java.io.ByteArrayInputStream
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import io.bullet.borer.Cbor

class MintingPolicyExampleSpec extends BaseValidatorSpec {

  case class TxInInfoTxOutRefOnly(txInInfoOutRef: TxOutRef)
  given Data.FromData[TxInInfoTxOutRefOnly] = (d: Data) =>
    val pair = Builtins.unsafeDataAsConstr(d)
    new TxInInfoTxOutRefOnly(summon[Data.FromData[TxOutRef]](pair.snd.head))

  object OptimizedMintingPolicy {
    import List.*

    /* Here we use a custom ScriptContext deserializer
       to avoid deserializing from Data fields that are not used in the script.
       This saves us more than 1000 bytes of the script size.
     */
    def mintingPolicyScript(
        txId: ByteString,
        txOutIdx: BigInt,
        tokensToMint: List[(ByteString, BigInt)]
    ) = (redeemer: Unit, ctxData: Data) => {
      val txInfoData = fieldAsData[ScriptContext](_.scriptContextTxInfo)(ctxData)
      val txInfoInputs =
        summon[Data.FromData[List[TxInInfoTxOutRefOnly]]](
          fieldAsData[TxInfo](_.txInfoInputs)(txInfoData)
        )
      val minted =
        summon[Data.FromData[Value]](fieldAsData[TxInfo](_.txInfoMint).apply(txInfoData))
      val ownSymbol =
        val purpose = fieldAsData[ScriptContext](_.scriptContextPurpose)(ctxData)
        val pair = Builtins.unsafeDataAsConstr(purpose)
        val tag = pair.fst
        val args = pair.snd
        if tag === BigInt(0) then Builtins.unsafeDataAsB(args.head)
        else throw new Exception("Not a minting policy")

      def equalsAssets(a: List[(ByteString, BigInt)], b: List[(ByteString, BigInt)]) = {
        List.foldLeft(a, true) { (acc, asset) =>
          asset match
            case (tokenName, amount) =>
              acc && List.exists(b) { asset =>
                asset match
                  case (tn, amt) => tn === tokenName && amt === amount
              }
        }
      }
      def ensureMinted(minted: Value): Unit = {
        List.findOrFail(minted) { asset =>
          asset match
            case (curSymbol, mintedTokens) =>
              if curSymbol === ownSymbol
              then equalsAssets(mintedTokens, tokensToMint)
              else false
        }
      }

      def ensureSpendsTxOut(inputs: List[TxInInfoTxOutRefOnly]): Unit = List.findOrFail(inputs) {
        txInInfo =>
          txInInfo.txInInfoOutRef match
            case TxOutRef(txOutRefTxId, txOutRefIdx) =>
              txOutRefTxId.hash === txId && txOutRefIdx === txOutIdx
      }
      ensureMinted(minted)
      ensureSpendsTxOut(txInfoInputs)
    }
    val compiledOptimizedMintingPolicyScript = compile(
      mintingPolicyScript(
        hoskyMintTxOutRef.txOutRefId.hash,
        hoskyMintTxOutRef.txOutRefIdx,
        List.Cons((ByteString.fromHex("484f534b59"), BigInt("1000000000000000")), List.Nil)
      )
    )

    val validator = new SimpleSirToUplcLowering().lower(compiledOptimizedMintingPolicyScript)
    val flatEncoded = ProgramFlatCodec.encodeFlat(Program((1, 0, 0), validator))
    val cbor = Cbor.encode(flatEncoded).toByteArray
    val cborHex = Utils.bytesToHex(Cbor.encode(flatEncoded).toByteArray)
    val doubleCborHex = Utils.bytesToHex(Cbor.encode(cbor).toByteArray)
  }

  def scriptContext(txInfoInputs: scalus.Predef.List[TxInInfo], value: Value) =
    ScriptContext(
      TxInfo(
        txInfoInputs = txInfoInputs,
        txInfoOutputs = scalus.Predef.List.Nil,
        txInfoFee = Value.lovelace(BigInt("188021")),
        txInfoMint = value,
        txInfoDCert = scalus.Predef.List.Nil,
        txInfoWdrl = scalus.Predef.List.Nil,
        txInfoValidRange = Interval.always,
        txInfoSignatories = scalus.Predef.List.Nil,
        txInfoData = scalus.Predef.List.Nil,
        txInfoId = TxId(hex"1e0612fbd127baddfcd555706de96b46c4d4363ac78c73ab4dee6e6a7bf61fe9")
      ),
      ScriptPurpose.Minting(hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235")
    )

  def performMintingPolicyValidatorChecks(validator: Term) = {
    import Data.toData
    def appliedScript(ctx: ScriptContext) = Program((1, 0, 0), validator $ () $ ctx.toData)

    def withScriptContext(txInfoInputs: scalus.Predef.List[TxInInfo], value: Value) =
      appliedScript(scriptContext(txInfoInputs, value))

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

    assertSameResult(Expected.Failure("Haven't spent expected TxOutRef"))(
      withScriptContext(
        List.empty,
        Value(
          hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235",
          hex"484f534b59",
          BigInt("1000000000000000")
        )
      )
    )
  }

  test("Minting Policy Validator") {
    import ScriptPurpose.*
    def mintingPolicyScript(
        txId: ByteString,
        txOutIdx: BigInt,
        tokenName: ByteString,
        amount: BigInt,
        redeemer: Unit,
        ctxData: Data
    ): Unit = {
      val ctx = summon[Data.FromData[ScriptContext]](ctxData)
      val txInfo = ctx.scriptContextTxInfo
      val txInfoInputs = txInfo.txInfoInputs
      val minted = txInfo.txInfoMint
      val purpose = ctx.scriptContextPurpose
      val ownSymbol = purpose match
        case Minting(curSymbol) => curSymbol
        case Spending(txOutRef) => throw new RuntimeException("Spending context is not supported")
        case Rewarding(stakingCred) =>
          throw new RuntimeException("Rewarding context is not supported")
        case Certifying(cert) => throw new RuntimeException("Certifying context is not supported")

      def findOrFail[A](lst: List[A])(p: A => Boolean): Unit = lst match
        case Nil              => throw new Exception("Not found")
        case Cons(head, tail) => if p(head) then () else findOrFail(tail)(p)

      def findToken(tokens: List[(ByteString, BigInt)]): Unit =
        findOrFail(tokens) { token =>
          token match
            case (tn, amt) => tn === tokenName && amt === amount
        }

      def ensureMinted(minted: Value): Unit = {
        findOrFail(minted) { asset =>
          asset match
            case (curSymbol, tokens) =>
              if curSymbol === ownSymbol
              then
                findOrFail(tokens) { tokens =>
                  tokens match
                    case (tn, amt) => tn === tokenName && amt === amount
                }
                true
              else false
        }
      }

      def ensureSpendsTxOut(inputs: List[TxInInfo]): Unit = findOrFail(inputs) { txInInfo =>
        txInInfo.txInInfoOutRef match
          case TxOutRef(txOutRefTxId, txOutRefIdx) =>
            txOutRefTxId.hash === txId && txOutRefIdx === txOutIdx
      }
      ensureMinted(minted)
      ensureSpendsTxOut(txInfoInputs)
    }

    val compiled = compile(
      mintingPolicyScript(
        hoskyMintTxOutRef.txOutRefId.hash,
        hoskyMintTxOutRef.txOutRefIdx,
        ByteString.fromHex("484f534b59"),
        BigInt("1000000000000000"),
        _,
        _
      )
    )
    // println(compiled.pretty.render(100))
    val validator = new SimpleSirToUplcLowering().lower(compiled)
    val flatSize = ProgramFlatCodec.encodeFlat(Program((1, 0, 0), validator)).length
    assert(flatSize == 1721)
    performMintingPolicyValidatorChecks(validator)
  }

  test("Minting Policy Validator Optimized") {
    val validator = OptimizedMintingPolicy.validator
    println(OptimizedMintingPolicy.compiledOptimizedMintingPolicyScript.pretty.render(100))
    println(OptimizedMintingPolicy.flatEncoded.length == 615)
    // assert(OptimizedMintingPolicy.flatEncoded.length == 615)
    // performMintingPolicyValidatorChecks(OptimizedMintingPolicy.validator)
    import Data.toData
    def appliedScript(ctx: ScriptContext) = Program((1, 0, 0), validator $ () $ ctx.toData)

    def withScriptContext(txInfoInputs: scalus.Predef.List[TxInInfo], value: Value) =
      appliedScript(scriptContext(txInfoInputs, value))

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

    assertSameResult(Expected.Failure("Wrong minted amount"))(
      withScriptContext(
        List(TxInInfo(hoskyMintTxOutRef, hoskyMintTxOut)),
        Value(hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235", hex"484f534b59", 2)
      )
    )

    assertSameResult(Expected.Failure("Unexpected tokens"))(
      withScriptContext(
        List(TxInInfo(hoskyMintTxOutRef, hoskyMintTxOut)),
        List.Cons(
          (
            hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235",
            List.Cons(
              (hex"484f534b59", BigInt("1000000000000000")),
              List.Cons((hex"deadbeef", BigInt("1000000000000000")), List.Nil)
            )
          ),
          List.Nil
        )
      )
    )
  }

  test("Size") {

    val compiled = compile {
      new TxOutRef(
        new TxId(ByteString.fromHex("a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235")),
        0
      )
    }
    val validator = new SimpleSirToUplcLowering().lower(compiled)
    val validatorReduced = Cek.evalUPLC(validator)
    println(compiled.pretty.render(100))
    println(validator.pretty.render(100))
    println(Cek.evalUPLC(validator).pretty.render(100))
    val flat = ProgramFlatCodec.encodeFlat(Program((1, 0, 0), validator))
    val flat2 = ProgramFlatCodec.encodeFlat(Program((1, 0, 0), validatorReduced))
    println(Utils.bytesToHex(flat))
    println(Utils.bytesToHex(flat2))
    println(flat.length - 3)
    println(flat2.length - 3)

    {
      val compiled = compile {
        ByteString.fromHex("a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235")
      }
      val validator = new SimpleSirToUplcLowering().lower(compiled)
      println(compiled.pretty.render(100))
      println(validator.pretty.render(100))
      val flat = ProgramFlatCodec.encodeFlat(Program((1, 0, 0), validator))
      println(Utils.bytesToHex(flat))
      println(flat.length - 3)
    }
    {
      val compiled = compile {
        BigInt(0)
      }
      val validator = new SimpleSirToUplcLowering().lower(compiled)
      println(compiled.pretty.render(100))
      println(validator.pretty.render(100))
      val flat = ProgramFlatCodec.encodeFlat(Program((1, 0, 0), validator))
      println(Utils.bytesToHex(flat))
      println(flat.length - 3)
    }
  }
}
