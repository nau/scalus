package scalus

import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.Compiler.fieldAsData
import scalus.builtins.Builtins
import scalus.builtins.ByteString
import scalus.builtins.ByteString.given
import scalus.ledger.api.v1.FromDataInstances.given
import scalus.ledger.api.v1.ToDataInstances.given
import scalus.ledger.api.v1.*
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
import scalus.uplc.Data.toData
import scalus.uplc.DefaultFun.*
import scalus.uplc.DefaultUni.Bool
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.{_, given}
import scalus.uplc.*
import scalus.utils.Utils

import java.io.ByteArrayInputStream
import java.nio.charset.Charset
import scala.collection.immutable
import scala.util.Failure
import scala.util.Success
import scala.util.Try

@Compile
object OptimizedPreimageValidator {

  def preimageValidator(datum: Data, redeemer: Data, ctxData: Data): Unit = {
    summon[FromData[(ByteString, ByteString)]](datum) match
      case (hash, pkh) =>
        val preimage = summon[FromData[ByteString]](redeemer)
        val signatories = summon[FromData[List[PubKeyHash]]](
          // deserialize only the signatories from the ScriptContext
          fieldAsData[ScriptContext](_.scriptContextTxInfo.txInfoSignatories)(ctxData)
        )

        List.findOrFail(signatories) { sig => sig.hash === pkh }
        if Builtins.sha2_256(preimage) === hash then ()
        else throw new RuntimeException("Wrong preimage")
  }
}

object OptimizedPreimage {
  val compiledOptimizedPreimageValidator = compile(OptimizedPreimageValidator.preimageValidator)
  val validator = new SimpleSirToUplcLowering().lower(compiledOptimizedPreimageValidator)
  val flatEncoded = ProgramFlatCodec.encodeFlat(Program((1, 0, 0), validator))
  val cbor = Cbor.encode(flatEncoded).toByteArray
  val cborHex = Utils.bytesToHex(Cbor.encode(flatEncoded).toByteArray)
  val doubleCborHex = Utils.bytesToHex(Cbor.encode(cbor).toByteArray)
}

class PreImageExampleSpec extends BaseValidatorSpec {

  def scriptContext(signatories: scalus.prelude.List[PubKeyHash]) =
    ScriptContext(
      TxInfo(
        txInfoInputs = scalus.prelude.List.Nil,
        txInfoOutputs = scalus.prelude.List.Nil,
        txInfoFee = Value.lovelace(BigInt("188021")),
        txInfoMint = Value.lovelace(BigInt("188021")),
        txInfoDCert = scalus.prelude.List.Nil,
        txInfoWdrl = scalus.prelude.List.Nil,
        txInfoValidRange = Interval.always,
        txInfoSignatories = signatories,
        txInfoData = scalus.prelude.List.Nil,
        txInfoId = TxId(hex"1e0612fbd127baddfcd555706de96b46c4d4363ac78c73ab4dee6e6a7bf61fe9")
      ),
      ScriptPurpose.Spending(hoskyMintTxOutRef)
    )

  def performChecks(validator: Term) = {
    def appliedScript(
        preimage: ByteString,
        pubKeyHash: PubKeyHash,
        hash: ByteString,
        signatories: scalus.prelude.List[PubKeyHash]
    ) =
      val datum = (hash, pubKeyHash.hash).toData
      val redeemer = preimage.toData
      val ctx = scriptContext(signatories)
      Program((1, 0, 0), validator $ datum $ redeemer $ ctx.toData)

    assertUplcEvalResult(Expected.Success(Const(Constant.Unit)))(
      appliedScript(
        preimage = ByteString.fromArray("Scalus rocks!".getBytes("UTF-8")),
        pubKeyHash = PubKeyHash(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6"),
        hash = hex"36c71edaf0affadf6dd9c5f8df3dc90ec0fc01bf9f8cf0f18268db24b2a3da49",
        signatories =
          List(PubKeyHash(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6"))
      )
    )

    assertUplcEvalResult(Expected.Failure("Wrong preimage"))(
      appliedScript(
        preimage = ByteString.fromArray("Scalus rocks!".getBytes("UTF-8")),
        pubKeyHash = PubKeyHash(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6"),
        hash = ByteString.fromHex("000000"),
        signatories =
          List(PubKeyHash(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6"))
      )
    )

    assertUplcEvalResult(Expected.Failure("No valid signature"))(
      appliedScript(
        preimage = ByteString.fromArray("Scalus rocks!".getBytes("UTF-8")),
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
      // check that the transaction is signed by the public key hash
      List.findOrFail(signatories) { sig => sig.hash === pkh }
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
    assert(flatEncoded.length == 1684)

    performChecks(validator)
  }

  test("Preimage Validator Optimized") {
    assert(OptimizedPreimage.flatEncoded.length == 274)
    performChecks(OptimizedPreimage.validator)
  }
}
