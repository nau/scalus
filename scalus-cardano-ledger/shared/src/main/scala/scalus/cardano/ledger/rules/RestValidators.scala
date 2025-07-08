package scalus.cardano.ledger
package rules

object MissingRequiredDatumsValidator extends STS.Validator {
    override final type Error = TransactionException

    override def validate(context: Context, state: State, event: Event): Result = {
        ???
    }
}

object ExactSetOfRedeemersValidator extends STS.Validator {
    override final type Error = TransactionException

    override def validate(context: Context, state: State, event: Event): Result = {
        ???
    }
}

object MetadataValidator extends STS.Validator {
    override final type Error = TransactionException

    override def validate(context: Context, state: State, event: Event): Result = {
        ???
    }
}

object ScriptsWellFormedValidator extends STS.Validator {
    override final type Error = TransactionException

    override def validate(context: Context, state: State, event: Event): Result = {
        ???
    }
}

object ProtocolParamsViewHashesMatchValidator extends STS.Validator {
    override final type Error = TransactionException

    override def validate(context: Context, state: State, event: Event): Result = {
        ???
    }
}

object OutputBootAddrAttrsTooBigValidator extends STS.Validator {
    override final type Error = TransactionException

    override def validate(context: Context, state: State, event: Event): Result = {
        ???
    }
}

object WrongNetworkValidator extends STS.Validator {
    override final type Error = TransactionException

    override def validate(context: Context, state: State, event: Event): Result = {
        ???
    }
}

object WrongNetworkWithdrawalValidator extends STS.Validator {
    override final type Error = TransactionException

    override def validate(context: Context, state: State, event: Event): Result = {
        ???
    }
}

object WrongNetworkInTxBodyValidator extends STS.Validator {
    override final type Error = TransactionException

    override def validate(context: Context, state: State, event: Event): Result = {
        ???
    }
}

object TooManyCollateralInputsValidator extends STS.Validator {
    override final type Error = TransactionException

    override def validate(context: Context, state: State, event: Event): Result = {
        ???
    }
}
