package scalus.cardano.ledger.txbuilder

import monocle.Focus.focus
import monocle.Lens
import scalus.cardano.ledger.utils.{MinCoinSizedTransactionOutput, MinTransactionFee, TxBalance}
import scalus.cardano.ledger.*

import scala.annotation.tailrec
import scala.util.Try

object LowLevelTxBuilder {
    class ChangeOutputDiffHandler(protocolParams: ProtocolParams, changeOutputIdx: Int) {
        def changeOutputDiffHandler(
            diff: Long,
            tx: Transaction
        ): Either[TxBalancingError, Transaction] = {
            val numOutputs = tx.body.value.outputs.size
            require(
              changeOutputIdx < numOutputs,
              s"Change output index $changeOutputIdx is out of bounds for outputs of size $numOutputs"
            )
            val changeOut = tx.body.value.outputs(changeOutputIdx)
            val changeLovelace = changeOut.value.value.coin.value
            val updatedLovelaceChange = changeLovelace + diff
            val newValue = changeOut.value.value
                .focus(_.coin.value)
                .replace(updatedLovelaceChange)
            val newChangeOut = Sized(changeOut.value.withValue(newValue))
            val minAda = MinCoinSizedTransactionOutput(newChangeOut, protocolParams)

            if updatedLovelaceChange < minAda.value then
                return Left(
                  TxBalancingError.InsufficientFunds(diff, minAda.value - updatedLovelaceChange)
                )

            val tb = tx.body.value
                .focus(_.outputs.index(changeOutputIdx))
                .replace(newChangeOut)
            val t = tx.copy(body = KeepRaw(tb))
            Right(t)
        }
    }

    /** Balances the transaction using a diff handler to adjust the transaction.
      *
      * Invariants:
      *   - only ADA is adjusted, native tokens must be balanced beforehand
      *   - fees never go below the initial fee
      */
    def balanceFeeAndChange(
        initial: Transaction,
        changeOutputIdx: Int,
        protocolParams: ProtocolParams,
        resolvedUtxo: Utxos,
        evaluator: PlutusScriptEvaluator,
    ): Either[TxBalancingError, Transaction] = {
        balanceFeeAndChange(
          initial,
          new ChangeOutputDiffHandler(protocolParams, changeOutputIdx).changeOutputDiffHandler,
          protocolParams,
          resolvedUtxo,
          evaluator
        )
    }

    /** Balances the transaction using a diff handler to adjust the transaction.
      *
      * Invariants:
      *   - only ADA is adjusted, native tokens must be balanced beforehand
      *   - fees never go below the initial fee
      */
    def balanceFeeAndChange(
        initial: Transaction,
        diffHandler: (Long, Transaction) => Either[TxBalancingError, Transaction],
        protocolParams: ProtocolParams,
        resolvedUtxo: Utxos,
        evaluator: PlutusScriptEvaluator,
    ): Either[TxBalancingError, Transaction] = {
        var iteration = 0

        @tailrec def loop(tx: Transaction): Either[TxBalancingError, Transaction] = {
            iteration += 1
            if iteration > 20 then return Left(TxBalancingError.CantBalance(0))
            val providedTxFee = tx.body.value.fee

            computeScriptsWitness(resolvedUtxo, evaluator, protocolParams)(tx) match
                case Left(error) => Left(error)
                case Right(txWithExUnits) =>
                    MinTransactionFee(txWithExUnits, resolvedUtxo, protocolParams) match
                        case Right(minFee) =>
                            // don't go below initial fee
                            val fee = Coin(math.max(minFee.value, initial.body.value.fee.value))
                            val txWithFees = setFee(fee)(txWithExUnits)

                            // if modifying the fee changed the transaction size - re-loop
                            if providedTxFee != fee then
                                MinTransactionFee(txWithFees, resolvedUtxo, protocolParams) match
                                    case Right(newMinFee) if newMinFee != minFee =>
                                        return loop(txWithFees)
                                    case Right(_) =>
                                    // Fee is still correct, continue
                                    case Left(error) => return Left(TxBalancingError.Failed(error))

                            // find the diff
                            val diff =
                                calculateChangeLovelace(txWithFees, resolvedUtxo, protocolParams)
                            // try to balance it
                            diffHandler(diff, txWithFees) match
                                case Right(balanced) =>
                                    val balancedDiff =
                                        calculateChangeLovelace(
                                          balanced,
                                          resolvedUtxo,
                                          protocolParams
                                        )
                                    // if diff is zero, we are done
                                    if balancedDiff == 0 then Right(balanced)
                                    else loop(balanced) // try again with the balanced tx
                                case Left(error) => Left(error)
                        case Left(error) => Left(TxBalancingError.Failed(error))
        }

        loop(initial)
    }

    private def computeScriptsWitness(
        utxos: Utxos,
        evaluator: PlutusScriptEvaluator,
        protocolParams: ProtocolParams
    )(tx: Transaction): Either[TxBalancingError, Transaction] = Try {
        val redeemers = evaluator.evalPlutusScripts(tx, utxos)
        setupRedeemers(protocolParams, tx, utxos, redeemers)
    }.toEither.left.map(t =>
        t match
            case psee: PlutusScriptEvaluationException => TxBalancingError.EvaluationFailed(psee)
            case other                                 => TxBalancingError.Failed(other)
    )

    private def setupRedeemers(
        protocolParams: ProtocolParams,
        tx: Transaction,
        utxos: Utxos,
        redeemers: Seq[Redeemer]
    ) = {
        if redeemers.isEmpty then {
            tx
        } else {
            val rawRedeemers = KeepRaw(Redeemers.from(redeemers))
            val txWithRedeemers =
                tx.copy(witnessSet = tx.witnessSet.copy(redeemers = Some(rawRedeemers)))
            val scriptDataHash =
                ScriptDataHashGenerator
                    .computeScriptDataHash(
                      txWithRedeemers,
                      utxos,
                      protocolParams,
                    )
                    .toTry
                    .get

            txWithRedeemers.copy(body =
                KeepRaw(tx.body.value.copy(scriptDataHash = scriptDataHash))
            )
        }
    }

}

// Transaction balancing error types
enum TxBalancingError {
    // Now it's only Plutus, but may become `Plutus... | SthElse...` in the future
    case EvaluationFailed(cause: PlutusScriptEvaluationException)
    // TODO: this constructor gets all other errors - rename?
    case Failed(cause: Throwable)
    case CantBalance(lastDiff: Long)
    case InsufficientFunds(diff: Long, minRequired: Long)
}

extension (t: TransactionOutput) {
    def valueLens: Lens[TransactionOutput, Value] =
        Lens[TransactionOutput, Value](_.value)(v => txout => txout.withValue(v))

    def withValue(amount: Value): TransactionOutput = t match {
        case shelley: TransactionOutput.Shelley =>
            shelley.copy(value = amount)
        case babbage: TransactionOutput.Babbage =>
            babbage.copy(value = amount)
    }
}

def modifyBody(tx: Transaction, f: TransactionBody => TransactionBody): Transaction = {
    val newBody = f(tx.body.value)
    tx.copy(body = KeepRaw(newBody))
}

def modifyWs(tx: Transaction, f: TransactionWitnessSet => TransactionWitnessSet): Transaction = {
    val newWs = f(tx.witnessSet)
    tx.copy(witnessSet = newWs)
}

def setFee(amount: Coin)(tx: Transaction) = modifyBody(tx, _.copy(fee = amount))

def calculateChangeLovelace(tx: Transaction, utxo: Utxos, params: ProtocolParams): Long = {
    val produced = TxBalance.produced(tx)
    val consumed = TxBalance.consumed(tx, CertState.empty, utxo, params).toTry.get
    consumed.coin.value - produced.coin.value
}

def calculateChangeValue(tx: Transaction, utxo: Utxos, params: ProtocolParams): Value = {
    val produced = TxBalance.produced(tx)
    val consumed = TxBalance.consumed(tx, CertState.empty, utxo, params).toTry.get
    consumed - produced
}
