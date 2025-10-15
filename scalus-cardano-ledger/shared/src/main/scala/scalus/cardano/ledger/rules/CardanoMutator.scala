package scalus.cardano.ledger
package rules

// WIP: It's a composition of Cardano ledger rules that mutate the state of the L1 ledger
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
            _ <- WrongNetworkValidator.validate(context, state, event)
            _ <- WrongNetworkWithdrawalValidator.validate(context, state, event)
            _ <- WrongNetworkInTxBodyValidator.validate(context, state, event)
            _ <- ExactSetOfRedeemersValidator.validate(context, state, event)
            _ <- ExUnitsTooBigValidator.validate(context, state, event)
            _ <- MetadataValidator.validate(context, state, event)
            _ <- MissingRequiredDatumsValidator.validate(context, state, event)
            _ <- OutputBootAddrAttrsSizeValidator.validate(context, state, event)
            _ <- ProtocolParamsViewHashesMatchValidator.validate(context, state, event)
            _ <- ScriptsWellFormedValidator.validate(context, state, event)
            _ <- TooManyCollateralInputsValidator.validate(context, state, event)
            state <- RemoveInputsFromUtxoMutator.transit(context, state, event)
            state <- AddOutputsToUtxoMutator.transit(context, state, event)
            state <- FeeMutator.transit(context, state, event)
            state <- PlutusScriptsTransactionMutator.transit(context, state, event)
        yield state
    }
}
