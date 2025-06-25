package scalus.cardano.ledger
package rules

import io.bullet.borer.Cbor

// It's Shelley.validateMaxTxSizeUTxO in cardano-ledger
object TransactionSizeValidator extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionSize = Cbor.encode(event).toByteArray.length
        val maxTransactionSize = context.env.params.maxTxSize
        if transactionSize > maxTransactionSize then
            return failure(
              IllegalArgumentException(
                s"Transaction size $transactionSize exceeds maximum allowed size $maxTransactionSize for transactionId ${event.id}"
              )
            )

        success
    }
}
