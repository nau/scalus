package scalus.cardano.ledger
package rules

object CardanoMutator extends STS.Mutator {
    override def transit(context: Context, state: State, event: Event): Result = {
        for
            _ <- EmptyInputsValidator.validate(context, state, event)
            _ <- InputsAndReferenceInputsDisjointValidator.validate(context, state, event)
            _ <- AllInputsMustBeInUtxoValidator.validate(context, state, event)
            _ <- ValueNotConservedUTxOValidator.validate(context, state, event)
            _ <- VerifiedWitnessesValidator.validate(context, state, event)
            _ <- NeededWitnessesValidator.validate(context, state, event)
            _ <- MissingScriptsValidator.validate(context, state, event)
            _ <- NativeScriptsValidator.validate(context, state, event)
            _ <- TransactionSizeValidator.validate(context, state, event)
            _ <- FeesOkValidator.validate(context, state, event)
            _ <- OutputTooSmallUTxOValidator.validate(context, state, event)
            _ <- OutputTooBigUTxOValidator.validate(context, state, event)
            _ <- OutsideValidityIntervalUTxOValidator.validate(context, state, event)
//            _ <- OutsideForecastValidator.validate(context, state, event)
            state <- RemoveInputsFromUtxoMutator.transit(context, state, event)
            state <- AddOutputsToUtxoMutator.transit(context, state, event)
            state <- FeeMutator.transit(context, state, event)
        yield state
    }
}
