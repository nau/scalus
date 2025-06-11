package scalus.cardano.ledger
package rules

object CardanoMutator extends STS.Mutator {
    override def transit(context: Context, state: State, event: Event): Result = {
        for
            _ <- EmptyInputsValidator.validate(context, state, event)
            _ <- InputsAndReferenceInputsDisjointValidator.validate(context, state, event)
            _ <- AllInputsMustBeInUtxoValidator.validate(context, state, event)
            _ <- EqualValidator.InputsAmountEqualsSumOfOutputsAmountAndFeeAmount.validate(
              context,
              state,
              event
            )
//            _ <- VerifiedWitnessesValidator.validate(context, state, event)
//            _ <- NeededWitnessesValidator.validate(context, state, event)
            state <- RemoveInputsFromUtxoMutator.transit(context, state, event)
            state <- AddOutputsToUtxoMutator.transit(context, state, event)
            state <- FeeMutator.transit(context, state, event)
        yield state
    }
}
