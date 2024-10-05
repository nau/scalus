package scalus
package examples

import scalus.Compiler.compile
import scalus.builtin.ByteString
import scalus.builtin.ByteString.given
import scalus.builtin.Data
import scalus.builtin.Data.toData
import scalus.ledger.api.v1
import scalus.ledger.api.v1.{PubKeyHash, TxId}
import scalus.ledger.api.v2.*
import scalus.ledger.api.v1.ToDataInstances.given
import scalus.ledger.api.v2.ToDataInstances.given
import scalus.prelude.List
import scalus.sir.RemoveRecursivity.removeRecursivity
import scalus.uplc.*
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.{*, given}
import scala.language.implicitConversions

class PreimageExampleSpec extends BaseValidatorSpec {
    import scalus.builtin.ToDataInstances.given

    private def scriptContext(signatories: scalus.prelude.List[PubKeyHash]) =
        ScriptContext(
          TxInfo(
            inputs = scalus.prelude.List.Nil,
            referenceInputs = scalus.prelude.List.Nil,
            outputs = scalus.prelude.List.Nil,
            fee = Value.lovelace(BigInt("188021")),
            mint = Value.lovelace(BigInt("188021")),
            dcert = scalus.prelude.List.Nil,
            withdrawals = scalus.prelude.AssocMap.empty,
            validRange = Interval.always,
            signatories = signatories,
            redeemers = scalus.prelude.AssocMap.empty,
            data = scalus.prelude.AssocMap.empty,
            id = TxId(hex"1e0612fbd127baddfcd555706de96b46c4d4363ac78c73ab4dee6e6a7bf61fe9")
          ),
          ScriptPurpose.Spending(hoskyMintTxOutRef)
        )

    private def performChecks(validator: Term) = {
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
        // compile to Scalus Intermediate Representation, SIR
        val compiled = compile(PreimageValidator.preimageValidator)
        // convert SIR to UPLC
        val validator = compiled.toUplc()
        val flatSize = Program((1, 0, 0), validator).flatEncoded.length
        assert(flatSize == 1664)

        performChecks(validator)
    }

    test("Optimized Preimage Validator") {
        val optV = OptimizedPreimage.compiledOptimizedPreimageValidator |> removeRecursivity
        val uplc = optV.toUplc()
        val program = Program((1, 0, 0), uplc)
        val flatSize = program.flatEncoded.length
        assert(flatSize == 162)
        performChecks(uplc)
    }
}
