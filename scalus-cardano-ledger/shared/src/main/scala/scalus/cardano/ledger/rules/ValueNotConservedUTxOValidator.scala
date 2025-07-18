package scalus.cardano.ledger
package rules

import scala.util.boundary
import scala.util.boundary.break

/** This is Shelley.validateValueNotConservedUTxO
  *
  * consumed pp utxo txb = produced pp poolParams txb
  */
object ValueNotConservedUTxOValidator extends STS.Validator {
    override final type Error = TransactionException.BadInputsUTxOException |
        TransactionException.ValueNotConservedUTxOException

    override def validate(context: Context, state: State, tx: Transaction): Result = boundary {
        val transactionId = tx.id
        val params = context.env.params
        val txBody = tx.body.value
        val mint = txBody.mint.getOrElse(MultiAsset.empty)

        val consumed = {
            val inputs = txBody.inputs
                .map { input =>
                    state.utxo.get(input) match {
                        case Some(output) => output.value
                        case None =>
                            break(
                              failure(TransactionException.BadInputsUTxOException(transactionId))
                            )
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
                state.certState.dstate.deposits.get(cred)
            }
            def lookupDRepDeposit(cred: Credential): Option[Coin] = {
                state.certState.vstate.dreps.get(cred).map(_.deposit)
            }

            // Compute the key deregistration refunds in a transaction
            val conwayTotalRefundsTxCerts =
                Certificate.shelleyTotalRefundsTxCerts(
                  lookupStakingDeposit,
                  params,
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
            conwayConsumed
        }

        val produced = {
            val burned = Value(
              Coin.zero,
              MultiAsset(mint.assets.map { case (policy, assets) =>
                  policy -> assets.filter((_, value) => value < 0)
              })
            )
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

        if consumed == produced then success
        else
            failure(
              TransactionException.ValueNotConservedUTxOException(transactionId, consumed, produced)
            )
    }
}
