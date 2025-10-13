package scalus.cardano.ledger.txbuilder

import scalus.cardano.address.Address
import scalus.cardano.ledger.{Utxos, Value}

/** Owns a set of pubkey-controlled UTXOs. */
trait Wallet {

    def selectInputs(required: Value): Option[Seq[(TransactionUnspentOutput, Witness)]]

    def utxo: Utxos

    def collateralInputs: Seq[(TransactionUnspentOutput, Witness)]

    def owner: Address
}

object Wallet {

    def empty(changeAddr: Address) = new Wallet {

        override def utxo: Utxos = Map.empty

        override def collateralInputs: Seq[(TransactionUnspentOutput, Witness)] = Seq.empty

        override def owner: Address = changeAddr

        override def selectInputs(
            required: Value
        ): Option[Seq[(TransactionUnspentOutput, Witness)]] = None
    }
}
