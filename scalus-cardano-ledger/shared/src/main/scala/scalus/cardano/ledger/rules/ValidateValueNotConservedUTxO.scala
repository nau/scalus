package scalus.cardano.ledger.rules

import scalus.cardano.ledger.*
import scalus.ledger.babbage.ProtocolParams

/** This is Shelley.validateValueNotConservedUTxO
  *
  * consumed pp utxo txb = produced pp poolParams txb
  */
object ValidateValueNotConservedUTxO extends STS.Validator {
    override def validate(context: Context, state: State, tx: Transaction): Result = {
        val params: ProtocolParams = ???
        val txBody = tx.body.value
        val mint = txBody.mint.getOrElse(Map.empty)

        val consumed = {
            val inputs = txBody.inputs
                .map { input =>
                    state.utxo.get(input) match {
                        case Some(output) => output.value
                        case None =>
                            throw IllegalArgumentException(s"Input $input not found in UTxO state")
                    }
                }
                .foldLeft(Value.zero)(_ + _)
            val withdrawals =
                txBody.withdrawals
                    .map { withdrawals =>
                        withdrawals.withdrawals.values.foldLeft(Coin.zero)(_ + _)
                    }
                    .getOrElse(Coin.zero)

            def lookupStakingDeposit(cred: Credential): Option[Coin] = ???
            def lookupDRepDeposit(cred: Credential): Option[Coin] = ???

            // Compute the key deregistration refunds in a transaction
            val conwayTotalRefundsTxCerts =
                Certificate.shelleyTotalRefundsTxCerts(
                  lookupStakingDeposit,
                  params,
                  txBody.certificates
                ) + Certificate
                    .conwayDRepRefundsTxCerts(
                      lookupDRepDeposit,
                      txBody.certificates
                    )
            val getTotalRefundsTxCerts = conwayTotalRefundsTxCerts
            val getTotalRefundsTxBody = getTotalRefundsTxCerts
            val refunds = getTotalRefundsTxBody

            // balance (txins tx ◁ u) + wbalance (txwdrls tx) + keyRefunds pp tx
            val consumedValue = inputs + Value(withdrawals + refunds)
            val minted = Value(
              Coin.zero,
              mint.map { case (policy, assets) =>
                  policy -> assets.filter((_, value) => value > 0)
              }
            )
            val getConsumedMaryValue = consumedValue + minted
            val conwayConsumed = getConsumedMaryValue
            conwayConsumed
        }

        val produced = {
            val burned = Value(
              Coin.zero,
              mint.map { case (policy, assets) =>
                  policy -> assets.filter((_, value) => value < 0)
              }
            )
            val getTotalDepositsTxCerts = Value.zero
            val conwayProposalsDeposits = Value.zero
            val deposits = getTotalDepositsTxCerts + conwayProposalsDeposits
            val outputs = txBody.outputs
                .map(_.value)
                .foldLeft(Value.zero)(_ + _)
            val fee = Value(txBody.fee)
            outputs + fee + deposits + burned
        }

        if consumed == produced then success
        else
            failure(
              IllegalArgumentException(
                s"Value not conserved: consumed $consumed, produced $produced"
              )
            )
    }
}
