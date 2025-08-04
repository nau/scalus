package scalus.cardano.ledger.txbuilder
import scalus.builtin.Data
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.utils.TxBalance.modifyBody
import scalus.cardano.ledger.utils.{OnSurplus, TxBalance}
import scalus.ledger.babbage.ProtocolParams

case class TxBuilder(
    utxo: UTxO,
    protocolParams: ProtocolParams,
    network: Network,
    tx: Transaction = TxBuilder.emptyTx,
    onSurplus: OnSurplus = OnSurplus.toFirstPayer,
) {

    def payToAddress(address: Address, value: Value): TxBuilder = {
        val out = Sized(TransactionOutput(address, value))
        copy(tx = modifyBody(tx, b => b.copy(outputs = b.outputs :+ out)))
    }
    
    def onSurplus(onSurplus: OnSurplus): TxBuilder = copy(onSurplus = onSurplus)

    def map(f: Transaction => Transaction): TxBuilder = copy(tx = f(tx))

    def signedBy(vk: VKeyWitness): TxBuilder = {
        val wSet = tx.witnessSet.copy(vkeyWitnesses = tx.witnessSet.vkeyWitnesses + vk)
        copy(tx = tx.copy(witnessSet = wSet))
    }

    def doFinalize = TxBalance.doBalance(tx)(utxo, protocolParams, onSurplus)

}

object TxBuilder {
    val emptyTx: Transaction = Transaction(
      TransactionBody(Set.empty, IndexedSeq.empty, Coin.zero),
      TransactionWitnessSet.empty
    )

    def withInputsFromUtxos(utxo: UTxO) = Transaction(
      TransactionBody(utxo.keySet, IndexedSeq.empty, Coin.zero),
      TransactionWitnessSet.empty
    )

    // Fetching these most likely involves effectful computations, `initialize` is an entry point to the pure API.
    def initialize(utxo: UTxO, protocolParams: ProtocolParams, network: Network): TxBuilder =
        TxBuilder(
          utxo,
          protocolParams,
          network,
          withInputsFromUtxos(utxo)
        )

    def modifyWs(
        tx: Transaction,
        f: TransactionWitnessSet => TransactionWitnessSet
    ): Transaction = {
        val newWs = f(tx.witnessSet)
        tx.copy(witnessSet = newWs)
    }
}

trait UtxoProvider {
    def utxos: UTxO
}
object UtxoProvider {
    def givenUtxos(u: UTxO): UtxoProvider = new UtxoProvider {
        override def utxos: UTxO = u
    }
}
