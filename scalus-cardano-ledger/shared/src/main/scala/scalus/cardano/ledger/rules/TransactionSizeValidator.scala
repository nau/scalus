package scalus.cardano.ledger
package rules

import scalus.Cbor

// It's Shelley.validateMaxTxSizeUTxO in cardano-ledger
object TransactionSizeValidator extends STS.Validator {
    override final type Error = TransactionException.InvalidTransactionSizeException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionId = event.id
        val transactionSize = Cbor.encode(event).length
        val maxTransactionSize = context.env.params.maxTxSize

        if transactionSize > maxTransactionSize then
            return failure(
              TransactionException.InvalidTransactionSizeException(
                transactionId,
                transactionSize,
                maxTransactionSize
              )
            )

        success
    }
}
