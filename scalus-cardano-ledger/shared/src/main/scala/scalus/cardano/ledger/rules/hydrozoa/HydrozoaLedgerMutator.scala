package scalus.cardano.ledger.rules.hydrozoa

import scalus.cardano.ledger.rules.*

object HydrozoaLedgerMutator extends Ledger.STS.Mutator[Ledger.StateI.All] {
    override def transit[StateT: StateI](
        context: Context,
        state: StateT,
        event: Event
    ): Either[Error, StateT] = {
        for
            _ <- EmptinessLedgerValidator.Inputs.validate(context, state, event)
            _ <- DisjointLedgerValidator.InputsAndReferenceInputs.validate(context, state, event)
            _ <- MustBeInUtxoLedgerValidator.AllInputs.validate(context, state, event)
            _ <- EqualLedgerValidator.InputsAmountEqualsSumOfOutputsAmountAndFeeAmount.validate(
              context,
              state,
              event
            )
            state <- RemoveInputsFromUtxoLedgerMutator.transit(context, state, event)
            state <- AddOutputsToUtxoLedgerMutator.transit(context, state, event)
        yield state
    }
}
