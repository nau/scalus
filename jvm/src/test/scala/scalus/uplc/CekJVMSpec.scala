package scalus.uplc

import org.bitcoins.crypto.ECPrivateKey
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import scalus.BaseValidatorSpec
import scalus.Compiler.compile
import scalus.Expected
import scalus.builtins.Builtins
import scalus.builtins.ByteString
import scalus.builtins.ByteString.given
import scalus.builtins.given
import scalus.ledger.api.v1.ToDataInstances.given
import scalus.ledger.api.v1.*
import scalus.prelude.List.Cons
import scalus.prelude.List.Nil
import scalus.toUplc
import scalus.uplc.DefaultFun.*
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.{*, given}
import scalus.utils.Utils
import scodec.bits.ByteVector

import java.security.SecureRandom
import scala.io.Source.fromFile

class CekJVMSpec extends BaseValidatorSpec:
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
            val isTxInfoOutputsEmpty =
                !DefaultFun.NullList $ (DefaultFun.UnListData $ txInfoOutputs)
            val result = !(!DefaultFun.IfThenElse $ isTxInfoOutputsEmpty $ ~() $ ~Error)
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

    test("verifyEd25519Signature") {
        val sir = compile { scalus.builtins.Builtins.verifyEd25519Signature }
        val verify = sir.toUplc()
        val valid = verify $
            hex"9518c18103cbdab9c6e60b58ecc3e2eb439fef6519bb22570f391327381900a8" $
            ByteString.fromString("hello") $
            hex"f13fa9acffb108114ec060561b58005fb2d69184de0a2d7400b2ea1f111c0794831cc832c92daf4807820dd9458324935e90bec855e8bf076bbbc4e42b727b07"

        assertSameResult(Expected.Success(true))(Program((1, 0, 0), valid))

        val wrongMessage = verify $
            hex"9518c18103cbdab9c6e60b58ecc3e2eb439fef6519bb22570f391327381900a8" $
            ByteString.fromString("NOT hello") $
            hex"f13fa9acffb108114ec060561b58005fb2d69184de0a2d7400b2ea1f111c0794831cc832c92daf4807820dd9458324935e90bec855e8bf076bbbc4e42b727b07"

        assertSameResult(Expected.Success(false))(Program((1, 0, 0), wrongMessage))

        val wrongPubKey = verify $
            hex"AA18c18103cbdab9c6e60b58ecc3e2eb439fef6519bb22570f391327381900a8" $
            ByteString.fromString("hello") $
            hex"f13fa9acffb108114ec060561b58005fb2d69184de0a2d7400b2ea1f111c0794831cc832c92daf4807820dd9458324935e90bec855e8bf076bbbc4e42b727b07"

        assertSameResult(Expected.Success(false))(Program((1, 0, 0), wrongPubKey))

        val wrongSignature = verify $
            hex"9518c18103cbdab9c6e60b58ecc3e2eb439fef6519bb22570f391327381900a8" $
            ByteString.fromString("NOT hello") $
            hex"FF3fa9acffb108114ec060561b58005fb2d69184de0a2d7400b2ea1f111c0794831cc832c92daf4807820dd9458324935e90bec855e8bf076bbbc4e42b727b07"

        assertSameResult(Expected.Success(false))(Program((1, 0, 0), wrongSignature))
    }

    test("verifyEcdsaSecp256k1Signature") {
        val sir = compile { scalus.builtins.Builtins.verifyEcdsaSecp256k1Signature }

        // Construct private key from hex
        val privateKey =
            ECPrivateKey("6846a082d76e7c34cd2deddc6ef3d4cb3220e6c72c7c9ec03408d60ed976837c")

        // Compute schnorr public key from private key
        val publicKey = privateKey.publicKey
        val publicKeyBytesCompressed = ByteString.fromArray(publicKey.bytes.toArray)

        val messageGen =
            Gen.containerOfN[Array, Byte](32, Arbitrary.arbitrary[Byte])
                .map(ByteString.unsafeFromArray)

        val wrongMessageGen =
            Gen
                .containerOf[Array, Byte](Arbitrary.arbitrary[Byte])
                .map(ByteString.unsafeFromArray)
                .suchThat(_.bytes.length != 32)

        val verify = sir.toUplc()
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
        val sir = compile { scalus.builtins.Builtins.verifySchnorrSecp256k1Signature }

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
                .suchThat(_.bytes.length != 32)

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
            // FIXME: these tests fail because current CEK implementation uses UnliftingImmediate as the default
            // and the uplc uses UnliftingDeferred.
            // TODO: uncomment this when we upgrade the CEK implementation
            // assertSameResult(Expected.Failure("invalidVk"))(Program((1, 0, 0), invalidVk))
            // assertSameResult(Expected.Failure("invalidData"))(Program((1, 0, 0), invalidData))
            // assertSameResult(Expected.Failure("invalidSignature"))(Program((1, 0, 0), invalidSignature))
        }
    }
