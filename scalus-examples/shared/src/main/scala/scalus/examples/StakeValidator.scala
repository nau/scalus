package scalus.examples

import scalus.*
import scalus.builtin.ByteString
import scalus.prelude.{*, given}
import scalus.ledger.api.v3.{Credential, Lovelace, Redeemer, ScriptPurpose, TxInfo}

object StakeValidator:
    def spend(
        withdrawScriptHash: ByteString,
        withdrawRedeemerValidator: (Redeemer, Lovelace) => Boolean,
        tx: TxInfo
    ): Boolean =
        val scriptCredential = Credential.ScriptCredential(withdrawScriptHash)
        val scriptPurpose = ScriptPurpose.Rewarding(scriptCredential)
        val redeemer = tx.redeemers.lookup(scriptPurpose).getOrFail(MissingRedeemer)
        val withdrawAmount = tx.withdrawals.lookup(scriptCredential).getOrFail(MissingWithdrawal)
        withdrawRedeemerValidator(redeemer, withdrawAmount)

    def spendMinimal(withdrawScriptHash: ByteString, tx: TxInfo): Boolean =
        val scriptCredential = Credential.ScriptCredential(withdrawScriptHash)
        tx.withdrawals.lookup(scriptCredential).isDefined

    def withdraw[T](
        withdrawalHandler: (T, ByteString, TxInfo) => Boolean,
        redeemer: T,
        credential: Credential,
        tx: TxInfo,
    ): Boolean =
        val validatorHash = credential match
            case Credential.ScriptCredential(validatorHash) => validatorHash
            case Credential.PubKeyCredential(_)             => fail(PubKeyCredentialNotSupported)

        withdrawalHandler(redeemer, validatorHash, tx)

    val MissingRedeemer = "There isn't a redeemer for the script purpose"
    val MissingWithdrawal = "There isn't a withdrawal for the script credential"
    val PubKeyCredentialNotSupported = "PubKeyCredential not supported"
