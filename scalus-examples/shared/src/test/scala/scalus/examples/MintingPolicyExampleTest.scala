package scalus
package examples

import scalus.Compiler.compile
import scalus.builtin.ByteString
import scalus.builtin.ByteString.*
import scalus.builtin.Data
import scalus.ledger.api.v1.*
import scalus.ledger.api.v2
import scalus.prelude.Option.*
import scalus.prelude.*
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.*

import scala.language.implicitConversions

class MintingPolicyExampleTest extends BaseValidatorTest {

    private def scriptContextV1(txInfoInputs: scalus.prelude.List[TxInInfo], value: Value) =
        ScriptContext(
          TxInfo(
            inputs = txInfoInputs,
            outputs = scalus.prelude.List.Nil,
            fee = Value.lovelace(BigInt("188021")),
            mint = value,
            dcert = scalus.prelude.List.Nil,
            withdrawals = scalus.prelude.List.Nil,
            validRange = Interval.always,
            signatories = scalus.prelude.List.Nil,
            data = scalus.prelude.List.Nil,
            id = TxId(hex"1e0612fbd127baddfcd555706de96b46c4d4363ac78c73ab4dee6e6a7bf61fe9")
          ),
          ScriptPurpose.Minting(hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235")
        )

    private def scriptContextV2(txInfoInputs: scalus.prelude.List[v2.TxInInfo], value: Value) =
        v2.ScriptContext(
          v2.TxInfo(
            inputs = txInfoInputs,
            referenceInputs = scalus.prelude.List.Nil,
            outputs = scalus.prelude.List.Nil,
            fee = Value.lovelace(BigInt("188021")),
            mint = value,
            dcert = scalus.prelude.List.Nil,
            withdrawals = SortedMap.empty,
            validRange = Interval.always,
            signatories = scalus.prelude.List.Nil,
            redeemers = SortedMap.empty,
            data = SortedMap.empty,
            id = TxId(hex"1e0612fbd127baddfcd555706de96b46c4d4363ac78c73ab4dee6e6a7bf61fe9")
          ),
          ScriptPurpose.Minting(hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235")
        )

    def withScriptContextV1(
        validator: Term,
        txInfoInputs: scalus.prelude.List[TxInInfo],
        value: Value
    ) =
        import Data.toData
        Program.plutusV1(validator $ () $ scriptContextV1(txInfoInputs, value).toData)

    def withScriptContextV2(
        validator: Term,
        txInfoInputs: scalus.prelude.List[TxInInfo],
        value: Value
    ) =
        import Data.toData
        val txInfoInputsV2 = txInfoInputs.map { case TxInInfo(txOutRef, txOut) =>
            val txOutV2 =
                v2.TxOut(txOut.address, txOut.value, v2.OutputDatum.NoOutputDatum, None)
            v2.TxInInfo(txOutRef, txOutV2)
        }
        Program.plutusV2(validator $ () $ scriptContextV2(txInfoInputsV2, value).toData)

    private def performMintingPolicyValidatorChecks(
        validator: Term
    )(withScriptContext: (Term, scalus.prelude.List[TxInInfo], Value) => Program) = {
        // The minting policy script should succeed when the TxOutRef is spent and the minted tokens are correct
        assertSameResult(Expected.Success(Const(Constant.Unit)))(
          withScriptContext(
            validator,
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
            validator,
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
            validator,
            List(TxInInfo(hoskyMintTxOutRef, hoskyMintTxOut)),
            Value(hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235", hex"484f534b59", 2)
          )
        )

        assertSameResult(Expected.Failure("Wrong Policy ID"))(
          withScriptContext(
            validator,
            List(TxInInfo(hoskyMintTxOutRef, hoskyMintTxOut)),
            Value(hex"cc", hex"484f534b59", BigInt("1000000000000000"))
          )
        )

        assertSameResult(Expected.Failure("Wrong Token Name"))(
          withScriptContext(
            validator,
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
            validator,
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
            validator,
            List(TxInInfo(hoskyMintTxOutRef, hoskyMintTxOut)),
            Value(
              hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235",
              hex"484f534b59",
              BigInt("1000000000000000")
            ) +
                Value(
                  hex"a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235",
                  hex"deadbeef",
                  BigInt("1000000000000000")
                )
          )
        )
    }

    val evaledTokens =
        val tokensSIR =
            compile(SortedMap.singleton(hex"484f534b59", BigInt("1000000000000000")))
        tokensSIR.toUplc().evaluate

    test("Minting Policy Validator") {
        val validator = MintingPolicy.compiledMintingPolicyScript.toUplc(
          generateErrorTraces = true,
          optimizeUplc = false
        )
        val appliedValidator =
            validator $ hoskyMintTxOutRef.id.hash $ hoskyMintTxOutRef.idx $ evaledTokens
        val flatSize = Program.plutusV1(appliedValidator).flatEncoded.length
        assert(flatSize == 2002)
        performMintingPolicyValidatorChecks(appliedValidator)(withScriptContextV1)
    }

    test("Minting Policy Validator V2") {
        val validator =
            MintingPolicyV2.compiledMintingPolicyScriptV2.toUplc(
              generateErrorTraces = true,
              optimizeUplc = false
            )
        val appliedValidator =
            validator $ hoskyMintTxOutRef.id.hash $ hoskyMintTxOutRef.idx $ evaledTokens
        val flatSize = Program.plutusV2(appliedValidator).flatEncoded.length
        assert(flatSize == 2154)
        performMintingPolicyValidatorChecks(appliedValidator)(withScriptContextV2)
    }

    test("Minting Policy Validator Optimized") {
        // println(MintingPolicy.compiledOptimizedMintingPolicyScript.pretty.render(100))
        val validator =
            MintingPolicy.compiledOptimizedMintingPolicyScript.toUplc(
              generateErrorTraces = true,
              optimizeUplc = false
            )
        val appliedValidator =
            validator $ hoskyMintTxOutRef.id.hash $ hoskyMintTxOutRef.idx $ evaledTokens
        val flatSize = Program.plutusV1(appliedValidator).flatEncoded.length
        assert(flatSize == 846)
        performMintingPolicyValidatorChecks(appliedValidator)(withScriptContextV1)
    }
}
