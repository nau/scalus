package scalus.cardano.ledger.rules

object EmptinessLedgerValidator {
    object Inputs extends LedgerValidator {
        override def validate(state: State, event: Event): Either[Error, Unit] = {
            if event.body.inputs.isEmpty then
                return Left(IllegalArgumentException("Empty transaction inputs"))

            Right(())
        }
    }

    object Outputs extends LedgerValidator {
        override def validate(state: State, event: Event): Either[Error, Unit] = {
            if event.body.outputs.isEmpty then
                return Left(IllegalArgumentException("Empty transaction outputs"))

            Right(())
        }
    }
}
