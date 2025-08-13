package scalus.cardano.ledger.utils
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionException.BadInputsUTxOException
import scalus.cardano.ledger.txbuilder.OnSurplus
import scalus.ledger.babbage.ProtocolParams

import scala.annotation.tailrec
import scala.util.boundary
import scala.util.boundary.break

object TxBalance {

    def consumed(
        tx: Transaction,
        certState: CertState,
        utxo: UTxO,
        protocolParams: ProtocolParams
    ): Either[BadInputsUTxOException, Value] = boundary {
        val txBody = tx.body.value
        val mint = txBody.mint.getOrElse(MultiAsset.empty)
        val inputs = txBody.inputs.view
            .map { input =>
                utxo.get(input) match {
                    case Some(output) => output.value
                    case None => break(Left(TransactionException.BadInputsUTxOException(tx.id)))
                }
            }
            .foldLeft(Value.zero)(_ + _)
        val withdrawals =
            txBody.withdrawals
                .map { withdrawals =>
                    withdrawals.withdrawals.values.foldLeft(Coin.zero)(_ + _)
                }
                .getOrElse(Coin.zero)

        def lookupStakingDeposit(cred: Credential): Option[Coin] = {
            certState.dstate.deposits.get(cred)
        }
        def lookupDRepDeposit(cred: Credential): Option[Coin] = {
            certState.vstate.dreps.get(cred).map(_.deposit)
        }

        // Compute the key deregistration refunds in a transaction
        val conwayTotalRefundsTxCerts =
            Certificate.shelleyTotalRefundsTxCerts(
              lookupStakingDeposit,
              protocolParams,
              txBody.certificates.toIndexedSeq
            ) + Certificate
                .conwayDRepRefundsTxCerts(
                  lookupDRepDeposit,
                  txBody.certificates.toIndexedSeq
                )
        val getTotalRefundsTxCerts = conwayTotalRefundsTxCerts
        // Compute the total refunds from the Certificates of a TransactionBody
        val getTotalRefundsTxBody = getTotalRefundsTxCerts
        val refunds = getTotalRefundsTxBody

        // balance (txins tx ◁ u) + wbalance (txwdrls tx) + keyRefunds pp tx
        val consumedValue = inputs + Value(withdrawals + refunds)
        val minted = Value(
          Coin.zero,
          MultiAsset(mint.assets.map { case (policy, assets) =>
              policy -> assets.filter((_, value) => value > 0)
          })
        )
        val getConsumedMaryValue = consumedValue + minted
        val conwayConsumed = getConsumedMaryValue
        Right(conwayConsumed)
    }

    def produced(tx: Transaction): Value = {
        val txBody = tx.body.value
        val mint = txBody.mint.getOrElse(MultiAsset.empty)
        val burned =
            val negativeMints = mint.assets.flatMap { case (policy, assets) =>
                val burns = assets.filter((_, value) => value < 0)
                if burns.isEmpty then None else Some(policy -> burns)
            }
            if negativeMints.isEmpty then Value.zero
            else Value(Coin.zero, MultiAsset(negativeMints))
        val outputs = txBody.outputs
            .map(_.value.value)
            .foldLeft(Value.zero)(_ + _)
        val shelleyTotalDepositsTxCerts: Coin = Coin.zero // FIXME: implement
        val conwayDRepDepositsTxCerts: Coin = Coin.zero // FIXME: implement
        val conwayTotalDepositsTxCerts = shelleyTotalDepositsTxCerts + conwayDRepDepositsTxCerts
        val getTotalDepositsTxBody = conwayTotalDepositsTxCerts
        val shelleyProducedValue = outputs + Value(txBody.fee + getTotalDepositsTxBody)
        val getProducedMaryValue = shelleyProducedValue + burned
        val conwayProducedValue =
            getProducedMaryValue + Value(txBody.donation.getOrElse(Coin.zero))
        val getProducedValue = conwayProducedValue
        getProducedValue
    }

    def doBalance(
        tx: Transaction
    )(utxo: UTxO, protocolParams: ProtocolParams, onSurplus: OnSurplus): Transaction = {
        val consumed = TxBalance.consumed(tx, CertState.empty, utxo, protocolParams).toTry.get
        val produced = TxBalance.produced(tx)
        if consumed.coin < produced.coin then {
            throw TransactionException.ValueNotConservedUTxOException(tx.id, consumed, produced)
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
                    throw TransactionException.IllegalArgumentException(
                      "Insufficient funds to cover transaction"
                    )
            }
        }
        val estimatedFee = MinTransactionFee(tx, utxo, protocolParams).toTry.get
        val initialTx = modifyBody(tx, _.copy(fee = estimatedFee))
        go(initialTx)
    }

    // need to refactor later, too many KeepRaw.apply calls
    def modifyBody(tx: Transaction, f: TransactionBody => TransactionBody): Transaction = {
        val newBody = f(tx.body.value)
        tx.copy(body = KeepRaw(newBody))
    }

    def doBalancePlutusScript(
        tx: Transaction,
        scriptEvaluator: PlutusScriptEvaluator
    )(utxo: UTxO, protocolParams: ProtocolParams, onSurplus: OnSurplus): Transaction = {
        val redeemers = scriptEvaluator.evalPlutusScripts(tx, utxo)
        val updatedWitnessSet = if redeemers.nonEmpty then {
            tx.witnessSet.copy(redeemers = Some(KeepRaw(Redeemers.from(redeemers))))
        } else {
            tx.witnessSet
        }
        val txWithEvaluatedScript = tx.copy(witnessSet = updatedWitnessSet)
        val baseFee = MinTransactionFee(txWithEvaluatedScript, utxo, protocolParams).toTry.get
        val scriptExecPrice = redeemers
            .map(r => scriptExecutionPrice(r.exUnits, protocolParams))
            .foldLeft(Coin.zero)(_ + _)
        val totalFee = Coin(baseFee.value + scriptExecPrice.value)

        /*
         * According to cip-40, we are required to return the excess collateral.
         * for now, todo,
         * and expect the caller to pass the collateral correctly
         * 
         * val returnCollatAdress = ???
         * val collateralReturn = if (totalCollateralValue > requiredCollateral) {
         *     val returnAmount = Coin(totalCollateralValue.value - requiredCollateral.value)
         *     Some(Sized(TransactionOutput(
         *         returnCollatAdress,
         *         Value(returnAmount)
         *     )))
         * } else None
         */

        val txWithFeeAndCollateral = modifyBody(
          txWithEvaluatedScript,
          body =>
              body.copy(
                fee = totalFee,
                totalCollateral =
                    if body.collateralInputs.nonEmpty then
                        Some(
                          body.collateralInputs.toSeq
                              .flatMap(utxo.get)
                              .map(_.value.coin)
                              .foldLeft(Coin.zero)(_ + _)
                        )
                    else None
              )
        )

        doBalance(txWithFeeAndCollateral)(utxo, protocolParams, onSurplus)
    }

    private def scriptExecutionPrice(
        executionUnits: ExUnits,
        protocolParams: ProtocolParams
    ): Coin = {
        val memoryPrice = protocolParams.executionUnitPrices.priceMemory
        val stepsPrice = protocolParams.executionUnitPrices.priceSteps
        val scriptCost = memoryPrice * executionUnits.memory + stepsPrice * executionUnits.steps
        Coin(scriptCost.toDouble.toLong)
    }
}
