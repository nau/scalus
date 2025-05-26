package scalus.cardano.ledger.rules

object DisjointRefInputsLedgerSTM extends LedgerSTM {
    override def transit(state: State, event: Event): Either[Error, State] = {
        val spentInputs = event.body.inputs
        val referenceInputs = event.body.referenceInputs.getOrElse(Set.empty)
        val intersection = referenceInputs.view.filter(spentInputs.contains)

        if intersection.nonEmpty then
            return Left(
              IllegalArgumentException(
                s"Inputs included in both reference inputs and spent inputs: intersection $intersection"
              )
            );

        Right(state)
    }
}
