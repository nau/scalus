package scalus.cardano.ledger
package rules

// It's part of Babbage.missingRequiredDatums in cardano-ledger
object MissingRequiredDatumsValidator extends STS.Validator {
    override final type Error = TransactionException

    override def validate(context: Context, state: State, event: Event): Result = {
        ???
    }
}

// It's part of Babbage.hasExactSetOfRedeemers in cardano-ledger
object ExactSetOfRedeemersValidator extends STS.Validator {
    override final type Error = TransactionException

    override def validate(context: Context, state: State, event: Event): Result = {
        ???
    }
}

// It's part of Babbage.validateMetadata in cardano-ledger
object MetadataValidator extends STS.Validator {
    override final type Error = TransactionException

    override def validate(context: Context, state: State, event: Event): Result = {
        ???
    }
}

// It's part of Babbage.validateScriptsWellFormed in cardano-ledger
object ScriptsWellFormedValidator extends STS.Validator {
    override final type Error = TransactionException

    override def validate(context: Context, state: State, event: Event): Result = {
        ???
    }
}

// It's part of Babbage.ppViewHashesMatch in cardano-ledger
object ProtocolParamsViewHashesMatchValidator extends STS.Validator {
    override final type Error = TransactionException

    override def validate(context: Context, state: State, event: Event): Result = {
        ???
    }
}
// It's part of Shelley.validateOutputBootAddrAttrsTooBig in cardano-ledger
object OutputBootAddrAttrsTooBigValidator extends STS.Validator {
    override final type Error = TransactionException

    override def validate(context: Context, state: State, event: Event): Result = {
        ???
    }
}
// It's part of Shelley.validateWrongNetwork in cardano-ledger
object WrongNetworkValidator extends STS.Validator {
    override final type Error = TransactionException

    override def validate(context: Context, state: State, event: Event): Result = {
        ???
    }
}
// It's part of Shelley.validateWrongNetworkWithdrawal in cardano-ledger
object WrongNetworkWithdrawalValidator extends STS.Validator {
    override final type Error = TransactionException

    override def validate(context: Context, state: State, event: Event): Result = {
        ???
    }
}
// It's part of Alonzo.validateWrongNetworkInTxBody in cardano-ledger
object WrongNetworkInTxBodyValidator extends STS.Validator {
    override final type Error = TransactionException

    override def validate(context: Context, state: State, event: Event): Result = {
        ???
    }
}
