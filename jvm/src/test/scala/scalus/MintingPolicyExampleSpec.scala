package scalus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.uplc.ArbitraryInstances
import scalus.builtins.ByteString.given
import scalus.ledger.api.v1.*
import scalus.ledger.api.v1.Instances.given
import scalus.uplc.Constant.Pair
import scalus.uplc.DefaultFun.*
import scalus.uplc.DefaultUni.{asConstant, Bool}
import scalus.uplc.ExprBuilder.fieldAsData1
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.{*, given}
import scalus.uplc.*
import scala.collection.immutable
import scalus.builtins.Builtins
import scalus.builtins.ByteString
import scalus.Predef.Maybe.*
import scalus.Predef.List
import scalus.Predef.List.{Cons, Nil}
import scalus.sir.SimpleSirToUplcLowering

class MintingPolicyExampleSpec
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with ArbitraryInstances {

  test("Minting Policy example") {
    import scalus.utils.Utils.*
    import ScriptPurpose.*

    // simple validator that checks that the spending transaction
    // has a signature of the given public key hash

    val txOutRef = TxOutRef(TxId(ByteString.fromHex("aa")), 1)
    val fakeTxOut = TxOut(
      txOutAddress = Address(Credential.PubKeyCredential(PubKeyHash(hex"deadbeef")), Nothing),
      Value.zero,
      Nothing
    )

    def mintingPolicyScript(
        txId: ByteString,
        txOutIdx: BigInt,
        tokenName: ByteString,
        amount: BigInt,
        redeemer: Unit,
        ctxData: Data
    ): Unit = {
      /* let
        -- see note [Obtaining the currency symbol]
        ownSymbol = V.ownCurrencySymbol ctx

        minted = V.txInfoMint txinfo
        expected = currencyValue ownSymbol c

        -- True if the pending transaction mints the amount of
        -- currency that we expect
        mintOK =
            let v = expected == minted
            in traceIfFalse "C0" {-"Value minted different from expected"-} v

        -- True if the pending transaction spends the output
        -- identified by @(refHash, refIdx)@
        txOutputSpent =
            let v = V.spendsOutput txinfo refHash refIdx
            in  traceIfFalse "C1" {-"Pending transaction does not spend the designated transaction output"-} v

    in mintOK && txOutputSpent */
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
      def findToken(tokens: List[(ByteString, BigInt)]): Unit =
        tokens match
          case Nil => throw new RuntimeException("Token not found")
          case Cons(token, tail) =>
            token match
              case (tn, amt) =>
                if tn == tokenName && amt == amount then () // TODO && amt == amount
                else findToken(tail)
      def ensureMinted(minted: Value): Unit = {
        minted match
          case Nil => throw new RuntimeException("Minted value is empty")
          case Cons(head, tail) =>
            head match
              case (curSymbol, tokens) =>
                if curSymbol == ownSymbol
                then findToken(tokens)
                else ensureMinted(tail)
      }
      def ensureSpendsTxOut(inputs: List[TxInInfo]): Unit = inputs match
        case Nil => throw new RuntimeException("TxInfoInputs is empty")
        case Cons(txInInfo, tail) =>
          if txOutRef.txOutRefId.hash == txId && txOutRef.txOutRefIdx == txOutIdx then ()
          else ensureSpendsTxOut(tail)
      ensureMinted(minted)
      ensureSpendsTxOut(txInfoInputs)
    }

    val compiled = ExprBuilder.compile(
      mintingPolicyScript(txOutRef.txOutRefId.hash, txOutRef.txOutRefIdx, ByteString.fromHex("deadbeef"), 1, _, _)
    )
    // val compiledTxOutRef = ExprBuilder.compile(txOutRef)

    println(compiled.pretty.render(100))
    // println(compiledTxOutRef.pretty.render(100))
    // val validator = Example.mintingPolicyScript(txOutRef, hex"deadbeef")
    val validator = Expr[Any](new SimpleSirToUplcLowering().lower(compiled))
    val flatBytes = ProgramFlatCodec.encodeFlat(Program(version = (1, 0, 0), term = validator.term))
    println(s"validator size ${flatBytes.length}")

//    println(validator.term.pretty.render(80))

    import Data.toData

    def scriptContext(txInfoInputs: scalus.Predef.List[TxInInfo], value: Value) =
      ScriptContext(
        TxInfo(
          txInfoInputs = txInfoInputs,
          txInfoOutputs = scalus.Predef.List.Nil,
          txInfoFee = Value.zero,
          txInfoMint = value,
          txInfoDCert = scalus.Predef.List.Nil,
          txInfoWdrl = scalus.Predef.List.Nil,
          txInfoValidRange = Interval.always,
          txInfoSignatories = scalus.Predef.List.Nil,
          txInfoData = scalus.Predef.List.Nil,
          txInfoId = TxId(hex"bb")
        ),
        ScriptPurpose.Minting(hex"ca")
      )

    def appliedScript(ctx: ScriptContext) =
//      println(ctx.toData)
      Program((1, 0, 0), validator.term $ () $ ctx.toData)

    assert(
      Cek.evalUPLCProgram(
        appliedScript(
          scriptContext(List(TxInInfo(txOutRef, fakeTxOut)), Value(hex"ca", hex"deadbeef", 1))
        )
      ) == Const(
        asConstant(())
      )
    )

    // TODO - add more tests
    /*
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
    assert(flatValidator.length == 286) */
  }
}
