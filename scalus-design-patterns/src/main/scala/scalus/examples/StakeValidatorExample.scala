package scalus.examples

import scalus.*
import scalus.builtin.Data
import scalus.ledger.api.v3.*
import scalus.patterns.StakeValidator
import scalus.prelude.*

// Example for a validator that requires a withdrawal from its script for each
// spend. Note that depending on an external script is typically more
// performant.
@Compile
object StakeValidatorExample extends Validator {
    override def spend(
        datum: Option[Data],
        redeemer: Redeemer,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val ownAddress = tx.inputs.find(_.outRef === ownRef).get.resolved.address
        val ownWithdrawal = ownAddress.credential match
            case Credential.ScriptCredential(validatorHash) => validatorHash
            case _ => fail("Own address must be ScriptCredential")

        StakeValidator.spend(
          withdrawalScriptHash = ownWithdrawal,
          withdrawalRedeemerValidator = (redeemer, lovelace) => lovelace === BigInt(0),
          txInfo = tx
        )
    }

    override def reward(redeemer: Redeemer, stakingKey: Credential, tx: TxInfo): Unit = {
        StakeValidator.withdraw(
          withdrawalValidator = (redeemer, validatorHash, txInfo) => true,
          redeemer = redeemer,
          credential = stakingKey,
          txInfo = tx
        )
    }
}
