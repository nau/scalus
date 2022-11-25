package scalus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.Compiler.compile
import scalus.Compiler.fieldAsData
import scalus.Predef.List
import scalus.Predef.List.Cons
import scalus.Predef.List.Nil
import scalus.Predef.Maybe.*
import scalus.Predef.*
import scalus.builtins.Builtins
import scalus.builtins.ByteString
import scalus.builtins.ByteString.given
import scalus.ledger.api.v1.Instances.given
import scalus.ledger.api.v1.*
import scalus.sir.SimpleSirToUplcLowering
import scalus.uplc.ArbitraryInstances
import scalus.uplc.Constant.Pair
import scalus.uplc.Data.FromData
import scalus.uplc.Data.toData
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
import java.nio.charset.Charset

class PreImageExampleSpec extends BaseValidatorSpec {

  def scriptContext(signatories: scalus.Predef.List[PubKeyHash]) =
    ScriptContext(
      TxInfo(
        txInfoInputs = scalus.Predef.List.Nil,
        txInfoOutputs = scalus.Predef.List.Nil,
        txInfoFee = Value.lovelace(BigInt("188021")),
        txInfoMint = Value.lovelace(BigInt("188021")),
        txInfoDCert = scalus.Predef.List.Nil,
        txInfoWdrl = scalus.Predef.List.Nil,
        txInfoValidRange = Interval.always,
        txInfoSignatories = signatories,
        txInfoData = scalus.Predef.List.Nil,
        txInfoId = TxId(hex"1e0612fbd127baddfcd555706de96b46c4d4363ac78c73ab4dee6e6a7bf61fe9")
      ),
      ScriptPurpose.Spending(hoskyMintTxOutRef)
    )

  def performChecks(validator: Term) = {
    def appliedScript(
        preimage: ByteString,
        pubKeyHash: PubKeyHash,
        hash: ByteString,
        signatories: scalus.Predef.List[PubKeyHash]
    ) =
      val datum = (hash, pubKeyHash).toData
      val redeemer = preimage.toData
      val ctx = scriptContext(signatories)
      Program((1, 0, 0), validator $ datum $ redeemer $ ctx.toData)

    assertUplcEvalResult(Expected.Success(Const(Constant.Unit)))(
      appliedScript(
        preimage = ByteString("Scalus rocks!".getBytes("UTF-8")),
        pubKeyHash = PubKeyHash(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6"),
        hash = hex"36c71edaf0affadf6dd9c5f8df3dc90ec0fc01bf9f8cf0f18268db24b2a3da49",
        signatories =
          List(PubKeyHash(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6"))
      )
    )

    assertUplcEvalResult(Expected.Failure("Wrong preimage"))(
      appliedScript(
        preimage = ByteString("Scalus rocks!".getBytes("UTF-8")),
        pubKeyHash = PubKeyHash(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6"),
        hash = ByteString.fromHex("000000"),
        signatories =
          List(PubKeyHash(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6"))
      )
    )

    assertUplcEvalResult(Expected.Failure("No valid signature"))(
      appliedScript(
        preimage = ByteString("Scalus rocks!".getBytes("UTF-8")),
        pubKeyHash = PubKeyHash(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6"),
        hash = hex"36c71edaf0affadf6dd9c5f8df3dc90ec0fc01bf9f8cf0f18268db24b2a3da49",
        signatories = List(PubKeyHash(hex"000000"))
      )
    )

  }

  test("Preimage Validator") {
    import Data.{fromData, given}

    def preimageValidator(datum: Data, redeemer: Data, ctxData: Data): Unit = {
      // deserialize from Data
      val (hash, pkh) = fromData[(ByteString, ByteString)](datum)
      val preimage = fromData[ByteString](redeemer)
      val ctx = fromData[ScriptContext](ctxData)
      // get the transaction signatories
      val signatories = ctx.scriptContextTxInfo.txInfoSignatories

      def findOrFail[A](lst: List[A])(p: A => Boolean): Unit = lst match
        case Nil              => throw new Exception("Not found")
        case Cons(head, tail) => if p(head) then () else findOrFail(tail)(p)
      // check that the transaction is signed by the public key hash
      findOrFail(signatories) { sig => sig.hash === pkh }
      // check that the preimage hashes to the hash
      if Builtins.sha2_256(preimage) === hash then ()
      else throw new RuntimeException("Wrong preimage")
      // throwing an exception compiles to UPLC error
    }
    // compile to Scalus Intermediate Representation, SIR
    val compiled = compile(preimageValidator)
    // convert SIR to UPLC
    val validator = new SimpleSirToUplcLowering().lower(compiled)
    val flatEncoded = ProgramFlatCodec.encodeFlat(Program((1, 0, 0), validator))
    assert(flatEncoded.length == 1550)

    performChecks(validator)
  }

  test("Preimage Validator Optimized") {
    import Data.{fromData, given}

    def preimageValidator(datum: Data, redeemer: Data, ctxData: Data): Unit = {
      summon[FromData[(ByteString, ByteString)]](datum) match
        case (hash, pkh) =>
          val preimage = summon[FromData[ByteString]](redeemer)
          val signatories = summon[FromData[List[PubKeyHash]]](
            // deserialize only the signatories from the ScriptContext
            fieldAsData[ScriptContext](_.scriptContextTxInfo.txInfoSignatories)(ctxData)
          )

          def findOrFail[A](lst: List[A])(p: A => Boolean): Unit = lst match
            case Nil              => throw new Exception("Not found")
            case Cons(head, tail) => if p(head) then () else findOrFail(tail)(p)

          findOrFail(signatories) { sig =>
            sig.hash === pkh
          }
          if Builtins.sha2_256(preimage) === hash then ()
          else throw new RuntimeException("Wrong preimage")
    }

    val compiled = compile(preimageValidator)
    println(compiled.pretty.render(100))
    val validator = new SimpleSirToUplcLowering().lower(compiled)
    val flatSize = ProgramFlatCodec.encodeFlat(Program((1, 0, 0), validator)).length
    assert(flatSize == 261)
    performChecks(validator)
  }
}
