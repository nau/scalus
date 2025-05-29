package scalus.cardano.ledger.rules

object InputsLedgerSTS extends LedgerSTS {
    override def transit(state: State, event: Event): Either[Error, State] = {
        for
            state <- EmptinessLedgerValidator.Inputs(state, event)
            state <- DisjointLedgerValidator.InputsAndReferenceInputs(state, event)
            state <- MustBeInUtxoLedgerValidator.AllInputs(state, event)
        yield state
    }
}
