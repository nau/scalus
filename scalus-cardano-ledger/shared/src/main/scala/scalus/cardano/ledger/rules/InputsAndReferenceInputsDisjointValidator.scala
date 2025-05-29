package scalus.cardano.ledger.rules

// inputs and refInputs are disjoint, in Conway and later Eras
// It's Babbage.disjointRefInputs in cardano-ledger
object InputsAndReferenceInputsDisjointValidator extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = {
        val inputs = event.body.value.inputs
        val referenceInputs = event.body.value.referenceInputs.getOrElse(Set.empty)
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
