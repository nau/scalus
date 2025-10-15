package scalus.cardano.ledger.txbuilder

import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.ledger.utils.MinCoinSizedTransactionOutput

object Change {

    def handleChange(
        diff: Long,
        tx: Transaction,
        changeAddress: Address,
        protocolParams: ProtocolParams
    ): Either[TxBalancingError, Transaction] = {
        val body = tx.body.value
        val changeOutputIdx = body.outputs.indexWhere(_.value.address == changeAddress)

        if diff == 0 then {
            if changeOutputIdx >= 0 && body.outputs(changeOutputIdx).value.value.coin.value == 0
            then {
                Right(
                  modifyBody(
                    tx,
                    b => b.copy(outputs = b.outputs.patch(changeOutputIdx, Nil, 1))
                  )
                )
            } else {
                Right(tx)
            }
        } else if diff > 0 then {
            if changeOutputIdx >= 0 then {
                updateChangeOutput(tx, changeOutputIdx, diff)
            } else {
                createChangeOutput(tx, changeAddress, diff)
            }
        } else {
            if changeOutputIdx >= 0 then {
                removeFromChangeOutput(tx, changeOutputIdx, -diff, protocolParams)
            } else {
                Left(TxBalancingError.InsufficientFunds(diff, -diff))
            }
        }
    }

    private def updateChangeOutput(
        tx: Transaction,
        changeOutputIdx: Int,
        amountToAdd: Long,
    ): Either[TxBalancingError, Transaction] = {
        val currentOutput = tx.body.value.outputs(changeOutputIdx).value
        val newCoinValue = currentOutput.value.coin.value + amountToAdd
        val newValue = currentOutput.value.copy(coin = Coin(newCoinValue))

        val updatedOutput = currentOutput match {
            case s: TransactionOutput.Shelley => s.copy(value = newValue)
            case b: TransactionOutput.Babbage => b.copy(value = newValue)
        }

        Right(
          modifyBody(
            tx,
            b => b.copy(outputs = b.outputs.updated(changeOutputIdx, Sized(updatedOutput)))
          )
        )
    }

    private def createChangeOutput(
        tx: Transaction,
        changeAddress: Address,
        amount: Long,
    ): Either[TxBalancingError, Transaction] = {
        val changeOutput: TransactionOutput = TransactionOutput.Babbage(
          address = changeAddress,
          value = Value(Coin(amount)),
          datumOption = None,
          scriptRef = None
        )

        Right(
          modifyBody(
            tx,
            b => b.copy(outputs = b.outputs.toSeq :+ Sized(changeOutput))
          )
        )
    }

    private def removeFromChangeOutput(
        tx: Transaction,
        changeOutputIdx: Int,
        amountToRemove: Long,
        protocolParams: ProtocolParams
    ): Either[TxBalancingError, Transaction] = {
        val currentOutput = tx.body.value.outputs(changeOutputIdx).value
        val newCoinValue = currentOutput.value.coin.value - amountToRemove

        // Check if output would still meet minAda requirement
        val minAda = MinCoinSizedTransactionOutput(
          tx.body.value.outputs(changeOutputIdx),
          protocolParams
        ).value

        if newCoinValue < minAda then {
            // Can't satisfy - change output would be below min ADA
            Left(TxBalancingError.CantBalance(-amountToRemove))
        } else {
            val newValue = currentOutput.value.copy(coin = Coin(newCoinValue))
            val updatedOutput = currentOutput match {
                case s: TransactionOutput.Shelley => s.copy(value = newValue)
                case b: TransactionOutput.Babbage => b.copy(value = newValue)
            }

            Right(
              modifyBody(
                tx,
                b => b.copy(outputs = b.outputs.updated(changeOutputIdx, Sized(updatedOutput)))
              )
            )
        }
    }
}
