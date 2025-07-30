package scalus.cardano.ledger.tx
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.utils.MinTransactionFee
import scalus.cardano.ledger.*
import scalus.ledger.babbage.ProtocolParams
import upickle.default.read

import scala.annotation.tailrec

case class TxBuilder(
    protocolParams: ProtocolParams,
    network: Network,
    soFar: Transaction = TxBuilder.emptyTx,
    utxoProvider: Option[UtxoProvider] = None,
) {

    private def utxos = utxoProvider.get.utxos

    def payTo(address: Address): PayTo = PayTo(address, this)

    def signedBy(vk: VKeyWitness): TxBuilder = {
        val wSet = soFar.witnessSet.copy(vkeyWitnesses = soFar.witnessSet.vkeyWitnesses + vk)
        copy(soFar = soFar.copy(witnessSet = wSet))
    }

    def balanceAndCalculateFees(changeAddress: Address): Transaction = {
        val consumed = calculateConsumedValue(soFar.body.value)
        val outputsValue = calculateProducedValue(soFar.body.value)
        
        if consumed.coin < outputsValue.coin then {
            throw new IllegalStateException(s"Insufficient funds: consumed ${consumed.coin}, needed at least ${outputsValue.coin}")
        }

        @tailrec
        def go(currentTx: Transaction): Transaction = {
            val txBody = currentTx.body.value
            val consumed = calculateConsumedValue(txBody)
            val outputsOnly = calculateProducedValue(txBody)
            val fee = txBody.fee
            val totalNeeded = Value(outputsOnly.coin + fee)
            val diff = consumed.coin - totalNeeded.coin

            if diff == Coin.zero then {
                currentTx
            } else if diff > Coin.zero then {
                val change = TransactionOutput(changeAddress, Value(diff))
                val newOutputs = txBody.outputs :+ Sized(change)
                val newTx = currentTx.copy(body = KeepRaw(txBody.copy(outputs = newOutputs)))
                
                MinTransactionFee(newTx, utxos, protocolParams) match {
                    case Right(correctFee) =>
                        val newTxWithFee = newTx.copy(body = KeepRaw(newTx.body.value.copy(fee = correctFee)))
                        val newOutputsValue = newTx.body.value.outputs.map(_.value.value).foldLeft(Value.zero)(_ + _)
                        val newTotalNeeded = Value(newOutputsValue.coin + correctFee)
                        if consumed.coin >= newTotalNeeded.coin then {
                            go(newTxWithFee)
                        } else {
                            val txWithoutChange = currentTx.copy(body = KeepRaw(txBody.copy(fee = correctFee)))
                            go(txWithoutChange)
                        }
                    case Left(error) =>
                        throw new IllegalStateException(
                          s"Failed to calculate fees: $error. tx: $currentTx"
                        )
                }
            } else {
                throw new IllegalStateException(s"Insufficient funds to cover outputs and fees: consumed ${consumed.coin}, needed ${totalNeeded.coin}")
            }
        }
        
        val initialTx = MinTransactionFee(soFar, utxos, protocolParams) match {
            case Right(estimatedFee) => 
                soFar.copy(body = KeepRaw(soFar.body.value.copy(fee = estimatedFee)))
            case Left(error) =>
                throw new IllegalStateException(s"Failed to calculate initial fees: $error")
        }
        
        go(initialTx)
    }

    private def calculateConsumedValue(txBody: TransactionBody): Value = txBody.inputs
        .flatMap(utxos.get)
        .foldLeft(Value.zero)((acc, v) => acc + v.value)

    private def calculateProducedValue(txBody: TransactionBody): Value =
        txBody.outputs
            .map(_.value.value)
            .foldLeft(Value.zero)(_ + _)
}

object TxBuilder {
    private val emptyTx: Transaction = Transaction(
      TransactionBody(Set.empty, IndexedSeq.empty, Coin.zero),
      TransactionWitnessSet.empty
    )
}

case class PayTo(
    destination: Address,
    builder: TxBuilder,
    count: Option[Value] = None,
    utxoProvider: Option[UtxoProvider] = None,
) {
    def amount(value: Value): PayTo = copy(count = Some(value))

    def using(utxos: UTxO): PayTo =
        copy(utxoProvider = Some(UtxoProvider.givenUtxos(utxos)))

    def using(address: Address): PayTo =
        copy(utxoProvider = Some(UtxoProvider.utxosFromAddress(address, targetValue = count)))

    def prepareTx: Either[String, TxBuilder] = {
        for {
            utxos <- utxoProvider.map(_.utxos).toRight("No UTXO provider specified")
            amount <- count.toRight("No amount specified")
        } yield {
            val output = TransactionOutput(destination, amount)
            val newOutputs = builder.soFar.body.value.outputs :+ Sized(output)
            val newInputs = builder.soFar.body.value.inputs ++ utxos.keys
            val newBody = builder.soFar.body.value.copy(
              inputs = newInputs,
              outputs = newOutputs
            )
            val newTx = builder.soFar.copy(body = builder.soFar.body.copy(value = newBody))
            builder.copy(soFar = newTx, utxoProvider = utxoProvider)
        }
    }

}

trait UtxoProvider {
    def utxos: UTxO
}
object UtxoProvider {
    def givenUtxos(u: UTxO): UtxoProvider = new UtxoProvider {
        override def utxos: UTxO = u
    }

    def utxosFromAddress(address: Address, targetValue: Option[Value]) =
        new UtxosByAddress(address, targetValue)
}

class UtxosByAddress(owner: Address, targetValue: Option[Value]) extends UtxoProvider {
    override def utxos: UTxO = Map.empty
}
