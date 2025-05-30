package scalus.cardano.ledger.rules

object InputsLedgerMutator extends Ledger.STS.Mutator[Ledger.StateI.All] {
    override def transit[StateT: StateI](state: StateT, event: Event): Either[Error, StateT] = {
        for
            _ <- EmptinessLedgerValidator.Inputs.validate(state, event)
            _ <- DisjointLedgerValidator.InputsAndReferenceInputs.validate(state, event)
            _ <- MustBeInUtxoLedgerValidator.AllInputs.validate(state, event)
            _ <- EqualLedgerValidator.InputsAmountEqualsSumOfOutputsAmountAndFeeAmount.validate(
              state,
              event
            )
            state <- RemoveInputsFromUtxoLedgerMutator.transit(state, event)
            state <- FeeLedgerMutator.transit(state, event)
        yield state
    }
}
