package scalus.cardano.ledger
package rules

// FIXME: Is validating and updating the UTxO state? What's equivalent in cardano-ledger?
object CardanoMutator extends STS.Mutator {
    override final type Error = TransactionException

    override def transit(context: Context, state: State, event: Event): Result = {
        for
            _ <- EmptyInputsValidator.validate(context, state, event)
            _ <- InputsAndReferenceInputsDisjointValidator.validate(context, state, event)
            _ <- AllInputsMustBeInUtxoValidator.validate(context, state, event)
            _ <- ValueNotConservedUTxOValidator.validate(context, state, event)
            _ <- VerifiedSignaturesInWitnessesValidator.validate(context, state, event)
            _ <- MissingKeyHashesValidator.validate(context, state, event)
            _ <- MissingOrExtraScriptHashesValidator.validate(context, state, event)
            _ <- NativeScriptsValidator.validate(context, state, event)
            _ <- TransactionSizeValidator.validate(context, state, event)
            _ <- FeesOkValidator.validate(context, state, event)
            _ <- OutputsHaveNotEnoughCoinsValidator.validate(context, state, event)
            _ <- OutputsHaveTooBigValueStorageSizeValidator.validate(context, state, event)
            _ <- OutsideValidityIntervalValidator.validate(context, state, event)
            _ <- OutsideForecastValidator.validate(context, state, event)
            state <- RemoveInputsFromUtxoMutator.transit(context, state, event)
            state <- AddOutputsToUtxoMutator.transit(context, state, event)
            state <- FeeMutator.transit(context, state, event)
        yield state
    }
}
