package scalus.cardano.ledger.txbuilder

import scalus.cardano.address.Address
import scalus.cardano.ledger.{UTxO, Value}

/** Owns a set of pubkey-controlled UTXOs. */
trait Wallet {

    def selectInputs(required: Value): Option[Seq[(TransactionUnspentOutput, Witness)]]

    def utxo: UTxO

    def collateralInputs: Seq[(TransactionUnspentOutput, Witness)]

    def changeAddress: Address
}

object Wallet {

    def empty(changeAddr: Address) = new Wallet {

        override def utxo: UTxO = Map.empty

        override def collateralInputs: Seq[(TransactionUnspentOutput, Witness)] = Seq.empty

        override def changeAddress: Address = changeAddr

        override def selectInputs(
            required: Value
        ): Option[Seq[(TransactionUnspentOutput, Witness)]] = None
    }
}
