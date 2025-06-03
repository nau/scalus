package scalus.cardano.ledger.rules

object EmptyInputsValidator extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = {
        if event.body.inputs.isEmpty then
            return failure(IllegalArgumentException("Empty transaction inputs"))

        success
    }
}
