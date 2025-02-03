package scalus.uplc

import org.bitcoins.crypto.ECPrivateKey
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import scalus.*
import scalus.BaseValidatorSpec
import scalus.Compiler.compile
import scalus.Expected
import scalus.builtin.{ByteString, Data, PlatformSpecific, given}
import scalus.builtin.ByteString.*
import scalus.ledger.api.v1.*
import scalus.ledger.api.v1.ToDataInstances.given
import scalus.prelude.List.Cons
import scalus.prelude.List.Nil
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.{*, given}
import scalus.uplc.eval.*
import scodec.bits.ByteVector

import scala.language.implicitConversions

class CekJVMSpec extends BaseValidatorSpec:

    test("simple validator example") {
        import TermDSL.*
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
            val isTxInfoOutputsEmpty =
                !DefaultFun.NullList $ (DefaultFun.UnListData $ txInfoOutputs)
            val result = !(!DefaultFun.IfThenElse $ isTxInfoOutputsEmpty $ ~() $ ~Error)
            result
        }
        assert(validator == Example.giftValidator.term)

        val program = validator.plutusV1.show

        val bytes = UplcCli.uplcToFlat(program)
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
        val appliedScript = validator.plutusV1 $ () $ () $ scriptContext.toData
        assert(appliedScript.deBruijnedProgram.evaluate == Const(asConstant(())))
    }

    test("PubKey Validator example") {
        import scalus.ledger.api.v1.*
        // simple validator that checks that the spending transaction
        // has a signature of the given public key hash
        val validator = Example.pubKeyValidator(PubKeyHash(hex"deadbeef"))

//    println(validator.term.show)

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
          appliedScript(
            scriptContext(Cons(PubKeyHash(hex"000000"), Cons(PubKeyHash(hex"deadbeef"), Nil)))
          ).evaluate == Const(asConstant(()))
        )

        assertThrows[EvaluationFailure](
          appliedScript(scriptContext(Cons(PubKeyHash(hex"000000"), Nil))).evaluate
              == Const(asConstant(()))
        )

        assertThrows[EvaluationFailure](
          appliedScript(scriptContext(Nil)).evaluate == Const(asConstant(()))
        )

        val flatValidator = UplcCli.uplcToFlat(Program((1, 0, 0), validator.term).show)
        assert(flatValidator.length == 95)
    }

    test("verifyEcdsaSecp256k1Signature") {
        val sir = compile { scalus.builtin.Builtins.verifyEcdsaSecp256k1Signature }

        // Construct private key from hex
        val privateKey =
            ECPrivateKey("6846a082d76e7c34cd2deddc6ef3d4cb3220e6c72c7c9ec03408d60ed976837c")

        // Compute schnorr public key from private key
        val publicKey = privateKey.publicKey
        val publicKeyBytesCompressed = ByteString.fromArray(publicKey.bytes.toArray)
        println(publicKeyBytesCompressed.toHex)

        val messageGen =
            Gen.containerOfN[Array, Byte](32, Arbitrary.arbitrary[Byte])
                .map(ByteString.unsafeFromArray)

        val wrongMessageGen =
            Gen
                .containerOf[Array, Byte](Arbitrary.arbitrary[Byte])
                .map(ByteString.unsafeFromArray)
                .suchThat(_.length != 32)

        val verify = sir.toUplc()

        val msg = summon[PlatformSpecific].sha2_256(ByteString.fromString("hello"))
        val sig =
            ByteString.fromArray(privateKey.sign(ByteVector(msg.bytes)).toRawRS.toArray)

        println(s"msg: ${msg.toHex}")
        println(s"sig: ${sig.toHex}")

        forAll(messageGen, wrongMessageGen) { (message, wrongMessage) =>
            // Create a signature

            val sig =
                ByteString.fromArray(privateKey.sign(ByteVector(message.bytes)).toRawRS.toArray)
            val valid = verify $ publicKeyBytesCompressed $ message $ sig

            val invalidVk = verify $
                ByteString.fromArray(publicKeyBytesCompressed.bytes.drop(1)) $
                message $
                sig

            val invalidData = verify $ publicKeyBytesCompressed $ wrongMessage $ sig

            val invalidSignature =
                verify $ publicKeyBytesCompressed $ message $ ByteString.fromArray(
                  sig.bytes.drop(1)
                )

            val wrongSig = privateKey.sign(
              ByteVector
                  .fromHex("0000000000000000000000000000000000000000000000000000000000000000")
                  .get
            )
            val wrongSignature = verify $ publicKeyBytesCompressed $ message $ ByteString.fromArray(
              wrongSig.toRawRS.toArray // wrong signature
            )

            assertSameResult(Expected.Success(true))(Program((1, 0, 0), valid))
            assertSameResult(Expected.Success(false))(Program((1, 0, 0), wrongSignature))
            assertSameResult(Expected.Failure("invalidVk"))(Program((1, 0, 0), invalidVk))
            assertSameResult(Expected.Failure("invalidData"))(Program((1, 0, 0), invalidData))
            assertSameResult(Expected.Failure("invalidSignature"))(
              Program((1, 0, 0), invalidSignature)
            )
        }
    }

    test("verifySchnorrSecp256k1Signature") {
        val sir = compile { scalus.builtin.Builtins.verifySchnorrSecp256k1Signature }

        // Construct private key from hex
        val privateKey =
            ECPrivateKey("6846a082d76e7c34cd2deddc6ef3d4cb3220e6c72c7c9ec03408d60ed976837c")

        // Compute schnorr public key from private key
        val publicKey = privateKey.schnorrPublicKey
        val publicKeyBytesCompressed =
            ByteString.fromArray(publicKey.bytes.toArray)

        val messageGen =
            Gen.containerOfN[Array, Byte](32, Arbitrary.arbitrary[Byte])
                .map(ByteString.unsafeFromArray)

        val wrongMessageGen =
            Gen
                .containerOf[Array, Byte](Arbitrary.arbitrary[Byte])
                .map(ByteString.unsafeFromArray)
                .suchThat(_.length != 32)

        val verify = sir.toUplc()
        forAll(messageGen, wrongMessageGen) { (message, wrongMessage) =>
            // Create a signature
            val signature = privateKey.schnorrSign(ByteVector(message.bytes))
            val sig = ByteString.fromArray(signature.bytes.toArray)
            val valid = verify $ publicKeyBytesCompressed $ message $ sig

            val invalidVk = verify $
                ByteString.fromArray(publicKeyBytesCompressed.bytes.drop(1)) $
                message $
                sig

            val invalidData = verify $ publicKeyBytesCompressed $ wrongMessage $ sig

            val invalidSignature =
                verify $ publicKeyBytesCompressed $ message $ ByteString.fromArray(
                  sig.bytes.drop(1)
                )

            val wrongSig = privateKey.schnorrSign(
              ByteVector
                  .fromHex("0000000000000000000000000000000000000000000000000000000000000000")
                  .get
            )
            val wrongSignature = verify $ publicKeyBytesCompressed $ message $ ByteString.fromArray(
              wrongSig.bytes.toArray // wrong signature
            )

            assertSameResult(Expected.Success(true))(Program((1, 0, 0), valid))
            assertSameResult(Expected.Success(false))(Program((1, 0, 0), wrongSignature))
            assertSameResult(Expected.Failure("invalidVk"))(Program((1, 0, 0), invalidVk))
            // FIXME: This test is failing because Bitcoin-s library only allows 32 byte messages
            // uncomment when fixed this: https://github.com/bitcoin-s/bitcoin-s/issues/5436
            // assertSameResult(Expected.Success(false))(Program((1, 0, 0), invalidData))
            assertSameResult(Expected.Failure("invalidSignature"))(
              Program((1, 0, 0), invalidSignature)
            )
        }
    }
