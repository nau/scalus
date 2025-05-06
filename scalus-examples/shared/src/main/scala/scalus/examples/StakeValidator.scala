package scalus.examples

import scalus.*
import scalus.prelude.*
import scalus.ledger.api.v3.{Credential, Lovelace, Redeemer, ScriptPurpose, TxInfo, ValidatorHash}

@Compile
object StakeValidator:
    def spend(
        withdrawalScriptHash: ValidatorHash,
        withdrawalRedeemerValidator: (Redeemer, Lovelace) => Boolean,
        txInfo: TxInfo
    ): Unit =
        val scriptCredential = Credential.ScriptCredential(withdrawalScriptHash)
        val scriptPurpose = ScriptPurpose.Rewarding(scriptCredential)

        val redeemer = txInfo.redeemers.get(scriptPurpose).getOrFail(MissingRedeemer)
        val withdrawalAmount =
            txInfo.withdrawals.get(scriptCredential).getOrFail(MissingWithdrawal)
        withdrawalRedeemerValidator(
          redeemer,
          withdrawalAmount
        ) orFail WithdrawalRedeemerValidatorFailed

    def spendMinimal(withdrawalScriptHash: ValidatorHash, txInfo: TxInfo): Unit =
        val scriptCredential = Credential.ScriptCredential(withdrawalScriptHash)
        txInfo.withdrawals.get(scriptCredential).getOrFail(MissingWithdrawal)

    def withdraw[T](
        withdrawalValidator: (T, ValidatorHash, TxInfo) => Boolean,
        redeemer: T,
        credential: Credential,
        txInfo: TxInfo
    ): Unit =
        val validatorHash = credential match
            case Credential.ScriptCredential(validatorHash) => validatorHash
            case Credential.PubKeyCredential(_)             => fail(PubKeyCredentialNotSupported)

        withdrawalValidator(redeemer, validatorHash, txInfo) orFail WithdrawalValidatorFailed

    inline val MissingRedeemer = "There isn't a redeemer for the script purpose"
    inline val MissingWithdrawal = "There isn't a withdrawal for the script credential"
    inline val WithdrawalRedeemerValidatorFailed = "Withdrawal redeemer validator failed"
    inline val WithdrawalValidatorFailed = "Withdrawal validator failed"
    inline val PubKeyCredentialNotSupported = "PubKeyCredential not supported"

end StakeValidator
