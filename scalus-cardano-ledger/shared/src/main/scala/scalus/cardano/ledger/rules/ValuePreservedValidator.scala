package scalus.cardano.ledger.rules

import scalus.cardano.ledger.*
import scalus.ledger.babbage.ProtocolParams

/** This is Shelley.validateValueNotConservedUTxO
  *
  * consumed pp utxo txb = produced pp poolParams txb
  */
object ValuePreservedValidator extends STS.Validator {
    override def validate(context: Context, state: State, tx: Transaction): Result = {
        val txBody = tx.body.value
        val mint = txBody.mint.getOrElse(Map.empty)
        val minted = Value(
          Coin.zero,
          mint.map { case (policy, assets) =>
              policy -> assets.filter((_, value) => value > 0)
          }
        )
        val burned = Value(
          Coin.zero,
          mint.map { case (policy, assets) =>
              policy -> assets.filter((_, value) => value < 0)
          }
        )

        val inputs = Value.zero
        val refunds = Value.zero
        val withdrawals = Value.zero
        val consumed = inputs + minted + refunds + withdrawals
        val getTotalDepositsTxCerts = Value.zero
        val conwayProposalsDeposits = Value.zero
        val deposits = getTotalDepositsTxCerts + conwayProposalsDeposits
        val outputs = Value.zero
        val fee = Value(txBody.fee)
        val produced = outputs + fee + deposits + burned
        if consumed == produced then success
        else
            failure(
              IllegalArgumentException(
                s"Value not conserved: consumed $consumed, produced $produced"
              )
            )
    }
}
