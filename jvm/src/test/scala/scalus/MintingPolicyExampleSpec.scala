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

class MintingPolicyExampleSpec
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with ArbitraryInstances {

  test("Minting Policy example") {
    import scalus.utils.Utils.*
    import ScriptPurpose.*

    // simple minting policy
    val txOutRef = TxOutRef(
      TxId(ByteString.fromHex("cdf25cc5278b8a583308f6536a537623adc84fa3f886ad573f5d29d0e7a40d43")),
      1
    )
    val fakeTxOut = TxOut(
      txOutAddress = Address(Credential.PubKeyCredential(PubKeyHash(hex"deadbeef")), Nothing),
      Value.zero,
      Nothing
    )

    case class TxInInfoTxOutRefOnly(txInInfoOutRef: TxOutRef)
    given Data.FromData[TxInInfoTxOutRefOnly] = (d: Data) =>
      val pair = Builtins.unsafeDataAsConstr(d)
      new TxInInfoTxOutRefOnly(summon[Data.FromData[TxOutRef]](pair.snd.head))

    def mintingPolicyScript(
        txId: ByteString,
        txOutIdx: BigInt,
        tokenName: ByteString,
        amount: BigInt,
        redeemer: Unit,
        ctxData: Data
    ): Unit = {
      /* val ctx = summon[Data.FromData[ScriptContext]](ctxData)
      val txInfo = ctx.scriptContextTxInfo
      val txInfoInputs = txInfo.txInfoInputs
      val minted = txInfo.txInfoMint
      val purpose = ctx.scriptContextPurpose
      val ownSymbol = purpose match
        case Minting(curSymbol) => curSymbol
        case Spending(txOutRef) => throw new RuntimeException("Spending context is not supported")
        case Rewarding(stakingCred) =>
          throw new RuntimeException("Rewarding context is not supported")
        case Certifying(cert) => throw new RuntimeException("Certifying context is not supported") */

      val txInfoData = fieldAsData[ScriptContext](_.scriptContextTxInfo)(ctxData)
      val txInfoInputs =
        summon[Data.FromData[List[TxInInfoTxOutRefOnly]]](
          fieldAsData[TxInfo](_.txInfoInputs)(txInfoData)
        )
      val minted = summon[Data.FromData[Value]](fieldAsData[TxInfo](_.txInfoMint).apply(txInfoData))
      val ownSymbol =
        val purpose = fieldAsData[ScriptContext](_.scriptContextPurpose)(ctxData)
        val pair = Builtins.unsafeDataAsConstr(purpose)
        val tag = pair.fst
        val args = pair.snd
        if tag === BigInt(0) then Builtins.unsafeDataAsB(args.head)
        else throw new Exception("Not a minting policy")

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

      def ensureSpendsTxOut(inputs: List[TxInInfoTxOutRefOnly]): Unit = findOrFail(inputs) {
        txInInfo =>
          val ref = txInInfo.txInInfoOutRef
          ref.txOutRefId.hash === txId && ref.txOutRefIdx === txOutIdx
      }
      ensureMinted(minted)
      ensureSpendsTxOut(txInfoInputs)
    }

    val compiled = compile(
      mintingPolicyScript(
        txOutRef.txOutRefId.hash,
        txOutRef.txOutRefIdx,
        ByteString.fromHex("deadbeef"),
        1,
        _,
        _
      )
    )
    println(compiled.pretty.render(100))
    val validator = new SimpleSirToUplcLowering().lower(compiled)

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
      Program((1, 0, 0), validator $ () $ ctx.toData)

    def evalFlat(program: Program): Term = {
      val deBruijned = DeBruijn.deBruijnProgram(Program((1, 0, 0), program.term))
      val flat = ProgramFlatCodec.encodeFlat(deBruijned)
      import scala.sys.process.*
      val cmd = "uplc evaluate --input-format flat"
      val out = cmd.#<(new ByteArrayInputStream(flat)).!!
      UplcParser.term.parse(out) match
        case Right(_, term) => term
        case Left(_)        => throw new EvaluationFailure("Failed to parse")
    }

    def assertSameResult(program: Program) = {
      val result1 = Try(evalFlat(program))
      val result2 = Try(Cek.evalUPLCProgram(program))
      println(s"$result1 == $result2")
      (result1, result2) match
        case (Success(term1), Success(term2)) => assert(term1 == term2)
        case (Failure(e1), Failure(e2))       => assert(true)
        case _                                => fail(s"Expected $result1 === $result2")
    }

    assertSameResult(
      appliedScript(
        scriptContext(List(TxInInfo(txOutRef, fakeTxOut)), Value(hex"ca", hex"deadbeef", 1))
      )
    )

    assertSameResult(
      appliedScript(
        scriptContext(List(TxInInfo(txOutRef, fakeTxOut)), Value(hex"ca", hex"deadbeef", 2))
      )
    )

    assertSameResult(
      appliedScript(
        scriptContext(List(TxInInfo(txOutRef, fakeTxOut)), Value(hex"cc", hex"deadbeef", 1))
      )
    )
  }
}
