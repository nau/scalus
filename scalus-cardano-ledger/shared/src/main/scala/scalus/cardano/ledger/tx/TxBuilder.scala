package scalus.cardano.ledger.tx
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.utils.{MinTransactionFee, TxBalance}
import scalus.ledger.babbage.ProtocolParams

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

    def balanceAndCalculateFees(changeAddress: Address): Either[TransactionException, Transaction] =
        TxBalance.consumed(soFar, CertState.empty, utxos, protocolParams).flatMap { consumed =>
            val produced = TxBalance.produced(soFar)
            if consumed.coin < produced.coin then {
                Left(
                  TransactionException.ValueNotConservedUTxOException(soFar.id, consumed, produced)
                )
            } else {

                @tailrec
                def go(currentTx: Transaction): Either[TransactionException, Transaction] = {
                    val txBody = currentTx.body.value
                    val currentProduced = TxBalance.produced(currentTx)
                    val diffLong = consumed.coin.value - currentProduced.coin.value
                    if diffLong == 0 then {
                        Right(currentTx)
                    } else if diffLong > 0 then {
                        val diff = Coin(diffLong)
                        val change = TransactionOutput(changeAddress, Value(diff))
                        val newOutputs = txBody.outputs :+ Sized(change)
                        val newTx =
                            currentTx.copy(body = KeepRaw(txBody.copy(outputs = newOutputs)))

                        MinTransactionFee(newTx, utxos, protocolParams) match {
                            case Right(correctFee) =>
                                val newTxWithFee = newTx
                                    .copy(body = KeepRaw(newTx.body.value.copy(fee = correctFee)))
                                val newProduced = TxBalance.produced(newTxWithFee)
                                if consumed.coin >= newProduced.coin then {
                                    go(newTxWithFee)
                                } else {
                                    val txWithoutChange =
                                        currentTx
                                            .copy(body = KeepRaw(txBody.copy(fee = correctFee)))
                                    go(txWithoutChange)
                                }
                            case Left(error) =>
                                Left(
                                  TransactionException
                                      .IllegalArgumentException(s"Failed to calculate fees: $error")
                                )
                        }
                    } else {
                        Left(
                          TransactionException
                              .IllegalArgumentException("Insufficient funds to cover transaction")
                        )
                    }
                }

                val initialTx = MinTransactionFee(soFar, utxos, protocolParams).left
                    .map(err =>
                        TransactionException
                            .IllegalArgumentException(s"Failed to calculate initial fees: $err")
                    )
                    .map(estimatedFee =>
                        soFar.copy(body = KeepRaw(soFar.body.value.copy(fee = estimatedFee)))
                    )

                initialTx.flatMap(go)
            }
        }
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

    private def prepareTx: Either[TransactionException, TxBuilder] = {
        for {
            utxos <- utxoProvider
                .map(_.utxos)
                .toRight(
                  TransactionException.IllegalArgumentException("No UTXO provider specified")
                )
            amount <- count.toRight(
              TransactionException.IllegalArgumentException("No amount specified")
            )
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

    def balanceAndCalculateFees(changeAddress: Address): Either[TransactionException, Transaction] =
        prepareTx.flatMap(_.balanceAndCalculateFees(changeAddress))

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
