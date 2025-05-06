package scalus.examples

import scalus.*
import scalus.prelude.{*, given}
import scalus.ledger.api.v3.{Redeemer, ScriptPurpose, TokenName, TxInfo, ValidatorHash}

@Compile
object TransactionLevelMinterValidator:
    def spend(
        minterScriptHash: ValidatorHash,
        minterRedeemerValidator: Redeemer => Boolean,
        minterTokensValidator: AssocMap[TokenName, BigInt] => Boolean,
        txInfo: TxInfo
    ): Unit =
        val scriptPurpose = ScriptPurpose.Minting(minterScriptHash)
        val tokens = txInfo.mint.get(minterScriptHash).getOrElse(AssocMap.empty)

        val redeemer: Redeemer = txInfo.redeemers.get(scriptPurpose).getOrFail(MissingRedeemer)
        minterRedeemerValidator(redeemer) orFail MinterRedeemerValidatorFailed
        minterTokensValidator(tokens) orFail MinterTokensValidatorFailed

    def spendMinimal(minterScriptHash: ValidatorHash, txInfo: TxInfo): Unit =
        txInfo.mint.get(minterScriptHash).getOrFail(MissingMint)

    inline val MissingRedeemer = "There isn't a redeemer for the script purpose"
    inline val MinterRedeemerValidatorFailed = "Minter redeemer validator failed"
    inline val MinterTokensValidatorFailed = "Minter tokens validator failed"
    inline val MissingMint = "There isn't a mint for the minter script hash"

end TransactionLevelMinterValidator
