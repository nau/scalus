package scalus.cardano.ledger
package rules

// inputs and refInputs are disjoint, in Conway and later Eras
// It's Babbage.disjointRefInputs in cardano-ledger
// {- inputs ∩ refInputs = ∅ -}
object InputsAndReferenceInputsDisjointValidator extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = {
//        ( pvMajor (pp ^. ppProtocolVersionL) > eraProtVerHigh @BabbageEra
//            && pvMajor (pp ^. ppProtocolVersionL) < natVersion @11
//        )
        val protocolVersionMajor = context.env.params.protocolVersion.major
        if protocolVersionMajor < babbageEraProtocolVersionMajor || protocolVersionMajor >= protocolVersionMajor11
        then return success

        val transactionId = event.id
        val inputs = event.body.value.inputs
        val referenceInputs = event.body.value.referenceInputs
        val intersection = inputs.intersect(referenceInputs)

        if intersection.nonEmpty then
            return failure(
              IllegalArgumentException(
                s"Inputs and reference inputs intersects: intersection $intersection for transactionId $transactionId"
              )
            );

        success
    }

    private val babbageEraProtocolVersionMajor = 8
    private val protocolVersionMajor11 = 11
}
