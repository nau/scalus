package scalus.examples.htlc

import scalus.builtin.ToData.*
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.ledger.Script.PlutusV3
import scalus.cardano.ledger.txbuilder.*
import scalus.ledger.api.v1.PosixTime

case class HtlcTransactions(env: Environment, wallet: Wallet) {

    val context = BuilderContext(env, wallet)
    val script = PlutusV3(HtlcValidator.script.cborByteString)
    val scriptAddress = Address(env.network, Credential.ScriptHash(script.scriptHash))

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

    def reveal(
        lockedUtxo: (TransactionInput, TransactionOutput),
        preimage: Preimage,
        recipientAddress: Address
    ): Either[String, Transaction] = {
        val (input, output) = lockedUtxo
        val redeemer = Action.Reveal(preimage).toData
        val datum = output.datumOption.flatMap {
            case DatumOption.Inline(d) => Some(d)
            case _                     => None
        }

        context.newTx
            .collectFrom(lockedUtxo, redeemer, script, datum)
            .payTo(recipientAddress, output.value)
            .complete()
    }

    def timeout(
        lockedUtxo: (TransactionInput, TransactionOutput),
        committerAddress: Address
    ): Either[String, Transaction] = {
        val (input, output) = lockedUtxo
        val redeemer = Action.Timeout.toData
        val datum = output.datumOption.flatMap {
            case DatumOption.Inline(d) => Some(d)
            case _                     => None
        }

        context.newTx
            .collectFrom(lockedUtxo, redeemer, script, datum)
            .payTo(committerAddress, output.value)
            .complete()
    }
}
