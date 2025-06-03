package scalus.cardano.ledger.rules

object InputsAndReferenceInputsDisjointValidator extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = {
        val inputs = event.body.inputs
        val referenceInputs = event.body.referenceInputs.getOrElse(Set.empty)
        val intersection = referenceInputs.view.filter(inputs.contains)

        if intersection.nonEmpty then
            return failure(
              IllegalArgumentException(
                s"Inputs and reference inputs intersects: intersection $intersection"
              )
            );

        success
    }
}
