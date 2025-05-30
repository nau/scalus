package scalus.cardano.ledger.rules

object EmptinessLedgerValidator {
    object Inputs extends Ledger.STS.Validator[Ledger.StateI] {
        override def validate[StateT: StateI](state: StateT, event: Event): Either[Error, Unit] = {
            if event.body.inputs.isEmpty then
                return Left(IllegalArgumentException("Empty transaction inputs"))

            Right(())
        }
    }

    object Outputs extends Ledger.STS.Validator[Ledger.StateI] {
        override def validate[StateT: StateI](state: StateT, event: Event): Either[Error, Unit] = {
            if event.body.outputs.isEmpty then
                return Left(IllegalArgumentException("Empty transaction outputs"))

            Right(())
        }
    }
}
