package scalus.cardano.ledger
package rules

// txins txb ≠ ∅
// It's Shelley.validateInputSetEmptyUTxO in cardano-ledger
object EmptyInputsValidator extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = {
        if event.body.value.inputs.isEmpty then
            return failure(
              IllegalArgumentException(s"Empty transaction inputs for transactionId ${event.id}")
            )

        success
    }
}
