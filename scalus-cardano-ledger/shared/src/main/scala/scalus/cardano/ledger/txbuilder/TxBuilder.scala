package scalus.cardano.ledger.txbuilder
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.txbuilder.TxBuilder.modifyBody
import scalus.cardano.ledger.utils.{MinTransactionFee, TxBalance}
import scalus.ledger.babbage.ProtocolParams

import scala.annotation.tailrec

case class TxBuilder(
    utxo: UTxO,
    protocolParams: ProtocolParams,
    network: Network,
    tx: Transaction = TxBuilder.emptyTx,
    onSurplus: OnSurplus = OnSurplus.toFirstPayer
) {

    def payToAddress(address: Address, value: Value): TxBuilder = {
        val body = tx.body.value
        val newBody = body.copy(outputs = body.outputs :+ Sized(TransactionOutput(address, value)))
        copy(tx = tx.copy(body = KeepRaw(newBody)))
    }

    def onSurplus(onSurplus: OnSurplus): TxBuilder = copy(onSurplus = onSurplus)

    def payToAddress(
        address: Address,
        value: Value,
        utxo: (TransactionInput, TransactionOutput)
    ): TxBuilder = {
        copy(utxo = this.utxo + utxo).payToAddress(address, value)
    }

    def map(f: Transaction => Transaction): TxBuilder = copy(tx = f(tx))

    def signedBy(vk: VKeyWitness): TxBuilder = {
        val wSet = tx.witnessSet.copy(vkeyWitnesses = tx.witnessSet.vkeyWitnesses + vk)
        copy(tx = tx.copy(witnessSet = wSet))
    }

    /** @throws TransactionException
      *   if the transaction could not be balanced
      */
    def balanceAndCalculateFees: Transaction = {
        val consumed = TxBalance.consumed(tx, CertState.empty, utxo, protocolParams).toTry.get
        val produced = TxBalance.produced(tx)
        if consumed.coin < produced.coin then {
            throw new TransactionException.ValueNotConservedUTxOException(tx.id, consumed, produced)
        }

        @tailrec
        def go(currentTx: Transaction): Transaction = {
            val currentProduced = TxBalance.produced(currentTx)
            val diffLong = consumed.coin.value - currentProduced.coin.value
            diffLong match {
                case d if d > 0L =>
                    val diff = Coin(d)
                    val newTx = onSurplus(utxo, diff)(currentTx)
                    val correctFee = MinTransactionFee(newTx, utxo, protocolParams).toTry.get
                    val newTxWithFee = modifyBody(newTx, _.copy(fee = correctFee))
                    val newProduced = TxBalance.produced(newTxWithFee)
                    if consumed.coin >= newProduced.coin then {
                        // fee is good
                        go(newTxWithFee)
                    } else {
                        // fee + change exceeds inputs, remove the change and rebalance again
                        val txWithoutChange =
                            modifyBody(currentTx, _.copy(fee = correctFee))
                        go(txWithoutChange)
                    }
                case 0L => currentTx
                case _ => // diff < 0, we cannot cover the tx
                    throw new TransactionException.IllegalArgumentException(
                      "Insufficient funds to cover transaction"
                    )
            }
        }
        val estimatedFee = MinTransactionFee(tx, utxo, protocolParams).toTry.get
        val initialTx = modifyBody(tx, _.copy(fee = estimatedFee))
        go(initialTx)
    }

}

object TxBuilder {
    private val emptyTx: Transaction = Transaction(
      TransactionBody(Set.empty, IndexedSeq.empty, Coin.zero),
      TransactionWitnessSet.empty
    )

    private def withInputsFromUtxos(utxo: UTxO) = Transaction(
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

    // need to refactor later, too many KeepRaw.apply calls
    def modifyBody(tx: Transaction, f: TransactionBody => TransactionBody): Transaction = {
        val newBody = f(tx.body.value)
        tx.copy(body = KeepRaw(newBody))
    }
}

trait OnSurplus {
    def apply(utxo: UTxO, surplus: Coin): Transaction => Transaction
}
object OnSurplus {
    import TxBuilder.modifyBody
    def toFee: OnSurplus = (_, surplus: Coin) =>
        tx => {
            val fee = tx.body.value.fee + surplus
            modifyBody(tx, _.copy(fee = fee))
        }

    def toAddress(address: Address): OnSurplus = (_, surplus: Coin) =>
        tx => {
            val changeOutput = TransactionOutput(address, Value(surplus))
            modifyBody(tx, b => b.copy(outputs = b.outputs :+ Sized(changeOutput)))
        }

    def toFirstPayer: OnSurplus = (utxo: UTxO, surplus: Coin) =>
        tx => {
            val firstPayer = utxo.head
            toAddress(firstPayer._2.address)(utxo, surplus)(tx)
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
