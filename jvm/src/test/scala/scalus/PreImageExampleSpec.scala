package scalus

import scalus.Compiler.{compile, fieldAsData}
import scalus.builtin.ByteString.given
import scalus.builtin.Data
import scalus.builtin.Data.{fromData, toData}
import scalus.builtin.FromDataInstances.given
import scalus.builtin.{Builtins, ByteString, given}
import scalus.ledger.api.v1.*
import scalus.ledger.api.v1.FromDataInstances.given
import scalus.ledger.api.v1.ToDataInstances.given
import scalus.prelude.{List, *}
import scalus.prelude.List.Nil
import scalus.prelude.Prelude.{===, given}
import scalus.uplc.*
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.{*, given}

@Compile
object OptimizedPreimageValidator {

    def preimageValidator(datum: Data, redeemer: Data, ctxData: Data): Unit = {
        fromData[(ByteString, ByteString)](datum) match
            case (hash, pkh) =>
                val preimage = fromData[ByteString](redeemer)
                val signatories = fromData[List[PubKeyHash]](
                  // deserialize only the signatories from the ScriptContext
                  fieldAsData[ScriptContext](_.txInfo.signatories)(ctxData)
                )

                List.findOrFail(signatories) { sig => sig.hash === pkh }
                if Builtins.sha2_256(preimage) === hash then ()
                else throw new RuntimeException("Wrong preimage")
    }
}

object OptimizedPreimage {
    val compiledOptimizedPreimageValidator = compile(OptimizedPreimageValidator.preimageValidator)
    val validator = compiledOptimizedPreimageValidator.toUplc()
    val programV1 = Program((1, 0, 0), validator)
    // val cbor = Cbor.encode(flatEncoded).toByteArray
    // val cborHex = Utils.bytesToHex(Cbor.encode(flatEncoded).toByteArray)
    val doubleCborHex = Program((1, 0, 0), validator).doubleCborHex
}

class PreImageExampleSpec extends BaseValidatorSpec {
    import scalus.builtin.ToDataInstances.given

    def scriptContext(signatories: scalus.prelude.List[PubKeyHash]) =
        ScriptContext(
          TxInfo(
            inputs = scalus.prelude.List.Nil,
            outputs = scalus.prelude.List.Nil,
            fee = Value.lovelace(BigInt("188021")),
            mint = Value.lovelace(BigInt("188021")),
            dcert = scalus.prelude.List.Nil,
            withdrawals = scalus.prelude.List.Nil,
            validRange = Interval.always,
            signatories = signatories,
            data = scalus.prelude.List.Nil,
            id = TxId(hex"1e0612fbd127baddfcd555706de96b46c4d4363ac78c73ab4dee6e6a7bf61fe9")
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
            val datum = (hash, pubKeyHash).toData
            val redeemer = preimage.toData
            val ctx = scriptContext(signatories)
            Program((1, 0, 0), validator $ datum $ redeemer $ ctx.toData)

        assertSameResult(Expected.Success(Const(Constant.Unit)))(
          appliedScript(
            preimage = ByteString.fromArray("Scalus rocks!".getBytes("UTF-8")),
            pubKeyHash =
                PubKeyHash(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6"),
            hash = hex"36c71edaf0affadf6dd9c5f8df3dc90ec0fc01bf9f8cf0f18268db24b2a3da49",
            signatories =
                List(PubKeyHash(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6"))
          )
        )

        assertSameResult(Expected.Failure("Wrong preimage"))(
          appliedScript(
            preimage = ByteString.fromArray("Scalus rocks!".getBytes("UTF-8")),
            pubKeyHash =
                PubKeyHash(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6"),
            hash = hex"000000",
            signatories =
                List(PubKeyHash(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6"))
          )
        )

        assertSameResult(Expected.Failure("No valid signature"))(
          appliedScript(
            preimage = ByteString.fromArray("Scalus rocks!".getBytes("UTF-8")),
            pubKeyHash =
                PubKeyHash(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6"),
            hash = hex"36c71edaf0affadf6dd9c5f8df3dc90ec0fc01bf9f8cf0f18268db24b2a3da49",
            signatories = List(PubKeyHash(hex"000000"))
          )
        )

    }

    test("Preimage Validator") {
        import Data.fromData

        def preimageValidator(datum: Data, redeemer: Data, ctxData: Data): Unit = {
            // deserialize from Data
            val (hash, pkh) = fromData[(ByteString, ByteString)](datum)
            val preimage = fromData[ByteString](redeemer)
            val ctx = fromData[ScriptContext](ctxData)
            // get the transaction signatories
            val signatories = ctx.txInfo.signatories
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
        val validator = compiled.toUplc()
        val flatSize = Program((1, 0, 0), validator).flatEncoded.length
        assert(flatSize == 1586)

        performChecks(validator)
    }

    test("Preimage Validator Optimized") {
        val flatSize = OptimizedPreimage.programV1.flatEncoded.length
        assert(flatSize == 254)
        performChecks(OptimizedPreimage.validator)
    }
}
