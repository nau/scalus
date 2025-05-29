package scalus.cardano.ledger.rules

// txins txb ≠ ∅
// It's Shelley.validateInputSetEmptyUTxO in cardano-ledge
object EmptyInputsValidator extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = {
        if event.body.value.inputs.isEmpty then
            return failure(IllegalArgumentException("Empty transaction inputs"))

        success
    }
}
