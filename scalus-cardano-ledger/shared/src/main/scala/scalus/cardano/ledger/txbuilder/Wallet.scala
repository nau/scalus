package scalus.cardano.ledger.txbuilder

import scalus.cardano.ledger.{Coin, UTxO}

/** Owns a set of utxos.
  */
trait Wallet {
    def getInput(targetValue: Coin): Option[(ResolvedTxInput, OutputWitness)]
    def utxo: UTxO
    def inputs: Set[ResolvedTxInput]
    def collateralInputs: Set[ResolvedTxInput]
}
object Wallet {

    def empty = new Wallet {

        override def getInput(targetValue: Coin): Option[(ResolvedTxInput, OutputWitness)] =
            None

        override def utxo: UTxO = Map.empty

        override def inputs: Set[ResolvedTxInput] = Set.empty

        override def collateralInputs: Set[ResolvedTxInput] = Set.empty

    }
}
