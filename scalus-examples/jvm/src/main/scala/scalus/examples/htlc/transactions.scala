package scalus.examples.htlc

import scalus.builtin.ToData.{given, *}
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.ledger.Script.PlutusV3
import scalus.cardano.ledger.txbuilder.*
import scalus.ledger.api.v1.PosixTime

/** High-level HTLC transaction builders using the new fluent API.
  *
  * Demonstrates how to use the transaction builder for:
  *   - Locking funds with a datum
  *   - Revealing the preimage to unlock
  *   - Timing out to refund
  */
case class HtlcTransactions(env: Environment, wallet: Wallet) {

    val context = BuilderContext(env, wallet)
    val script = PlutusV3(HtlcValidator.script.cborByteString)
    val scriptAddress = Address(env.network, Credential.ScriptHash(script.scriptHash))

    /** Lock funds at the HTLC script with the contract datum.
      *
      * @param value
      *   The value to lock
      * @param committer
      *   The party who can claim after timeout
      * @param receiver
      *   The party who can claim with preimage
      * @param image
      *   The hash of the secret preimage
      * @param timeout
      *   The time after which committer can reclaim
      * @return
      *   Either an error or the built transaction
      */
    def lock(
        value: Value,
        committer: PubKeyHash,
        receiver: PubKeyHash,
        image: Image,
        timeout: PosixTime
    ): Either[String, Transaction] = {
        val datum = ContractDatum(committer, receiver, image, timeout).toData
        context.newTx
            .payToScript(scriptAddress, value, datum)
            .complete()
    }

    /** Reveal the preimage to unlock funds from the HTLC script.
      *
      * @param lockedUtxo
      *   The UTxO locked at the script
      * @param preimage
      *   The secret preimage
      * @param recipientAddress
      *   Where to send the unlocked funds
      * @return
      *   Either an error or the built transaction
      */
    def reveal(
        lockedUtxo: TransactionUnspentOutput,
        preimage: Preimage,
        recipientAddress: Address
    ): Either[String, Transaction] = {
        val redeemer = Action.Reveal(preimage).toData
        val datum = lockedUtxo.output.datumOption.flatMap {
            case DatumOption.Inline(d) => Some(d)
            case _                     => None
        }

        context.newTx
            .collectFrom(lockedUtxo, redeemer, script, datum)
            .payTo(recipientAddress, lockedUtxo.output.value)
            .complete()
    }

    /** Timeout transaction to reclaim funds after the deadline.
      *
      * @param lockedUtxo
      *   The UTxO locked at the script
      * @param committerAddress
      *   The committer's address to receive the refund
      * @return
      *   Either an error or the built transaction
      */
    def timeout(
        lockedUtxo: TransactionUnspentOutput,
        committerAddress: Address
    ): Either[String, Transaction] = {
        val redeemer = Action.Timeout.toData
        val datum = lockedUtxo.output.datumOption.flatMap {
            case DatumOption.Inline(d) => Some(d)
            case _                     => None
        }

        context.newTx
            .collectFrom(lockedUtxo, redeemer, script, datum)
            .payTo(committerAddress, lockedUtxo.output.value)
            .complete()
    }
}
