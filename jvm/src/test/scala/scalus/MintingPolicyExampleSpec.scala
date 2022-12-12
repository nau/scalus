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

  case class TxInInfoTxOutRefOnly(txInInfoOutRef: TxOutRef)
  given Data.FromData[TxInInfoTxOutRefOnly] = (d: Data) =>
    val pair = Builtins.unsafeDataAsConstr(d)
    new TxInInfoTxOutRefOnly(summon[Data.FromData[TxOutRef]](pair.snd.head))

  object MintingPolicy {
    import List.*
    import ScriptPurpose.*

    case class MintingContext(inputs: List[TxOutRef], minted: Value, ownSymbol: CurrencySymbol)

    val simpleCtxDeserializer: Data => MintingContext = (ctxData: Data) => {
      val ctx = summon[Data.FromData[ScriptContext]](ctxData)
      val txInfo = ctx.scriptContextTxInfo
      val txInfoInputs = txInfo.txInfoInputs
      val minted = txInfo.txInfoMint
      val purpose = ctx.scriptContextPurpose
      val ownSymbol = purpose match
        case Minting(curSymbol) => curSymbol
        case Spending(txOutRef) => throw new RuntimeException("PS")
        case Rewarding(stakingCred) =>
          throw new RuntimeException("PR")
        case Certifying(cert) => throw new RuntimeException("PC")
      new MintingContext(
        List.map(txInfoInputs)(_.txInInfoOutRef),
        minted,
        ownSymbol
      )
    }

    val optimizedCtxDeserializer: Data => MintingContext = (ctxData: Data) => {
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
        else throw new Exception("P")
      new MintingContext(
        List.map(txInfoInputs)(_.txInInfoOutRef),
        minted,
        ownSymbol
      )
    }
    /* Here we use a custom ScriptContext deserializer
       to avoid deserializing from Data fields that are not used in the script.
       This saves us more than 1000 bytes of the script size.
     */
    def mintingPolicyScript(
        txId: ByteString,
        txOutIdx: BigInt,
        tokensToMint: AssocMap[ByteString, BigInt],
        deserializer: Data => MintingContext
    ) = (redeemer: Unit, ctxData: Data) => {
      deserializer(ctxData) match
        case MintingContext(txOutRefs, minted, ownSymbol) =>
          val mintedTokens = AssocMap.lookup(minted)(ownSymbol) match
            case Just(mintedTokens) => mintedTokens
            case Nothing =>
              throw new Exception("T")

          val checkSpendsTxOut = List.find(txOutRefs) { case TxOutRef(txOutRefTxId, txOutRefIdx) =>
            txOutRefTxId.hash === txId && txOutRefIdx === txOutIdx
          }

          val check = (b: Boolean, msg: String) => if b then () else throw new Exception(msg)
          checkSpendsTxOut match
            // If the transaction spends the TxOut, then it's a minting transaction
            case Just(input) => check(Value.equalsAssets(mintedTokens, tokensToMint), "M")
            // Otherwise, it's a burn transaction
            case Nothing =>
              // check burned
              val burned = List.all(AssocMap.toList(mintedTokens)) { case (tokenName, amount) =>
                Builtins.lessThanInteger(amount, BigInt(0))
              }
              check(burned, "B")
    }

    val compiledOptimizedMintingPolicyScript = compile(
      mintingPolicyScript(
        hoskyMintTxOutRef.txOutRefId.hash,
        hoskyMintTxOutRef.txOutRefIdx,
        AssocMap.singleton(ByteString.fromHex("484f534b59"), BigInt("1000000000000000")),
        optimizedCtxDeserializer
      )
    )

    val compiledMintingPolicyScript = compile(
      mintingPolicyScript(
        hoskyMintTxOutRef.txOutRefId.hash,
        hoskyMintTxOutRef.txOutRefIdx,
        AssocMap.singleton(ByteString.fromHex("484f534b59"), BigInt("1000000000000000")),
        simpleCtxDeserializer
      )
    )

    val validator = new SimpleSirToUplcLowering(generateErrorTraces = true)
      .lower(compiledOptimizedMintingPolicyScript)
    val flatEncoded = ProgramFlatCodec.encodeFlat(Program((1, 0, 0), validator))
    val cbor = Cbor.encode(flatEncoded).toByteArray
    val cborHex = Utils.bytesToHex(Cbor.encode(flatEncoded).toByteArray)
    val doubleCborHex = Utils.bytesToHex(Cbor.encode(cbor).toByteArray)
  }

  def scriptContext(txInfoInputs: scalus.Prelude.List[TxInInfo], value: Value) =
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

  def performMintingPolicyValidatorChecks(validator: Term) = {
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

  test("Minting Policy Validator") {
    // println(compiled.pretty.render(100))
    val validator =
      new SimpleSirToUplcLowering(generateErrorTraces = true)
        .lower(MintingPolicy.compiledMintingPolicyScript)
    val flatSize = ProgramFlatCodec.encodeFlat(Program((1, 0, 0), validator)).length
    // println(flatSize)
    assert(flatSize == 2433)
    performMintingPolicyValidatorChecks(validator)
  }

  test("Minting Policy Validator Optimized") {
    val validator = MintingPolicy.validator
    // println(MintingPolicy.compiledOptimizedMintingPolicyScript.pretty.render(100))
    // println(MintingPolicy.validator.pretty.render(100))
    // println(MintingPolicy.flatEncoded.length)
    assert(MintingPolicy.flatEncoded.length == 1105)
    performMintingPolicyValidatorChecks(MintingPolicy.validator)
  }
}
