package scalus.cardano.ledger.rules

object DisjointLedgerValidator {
    object InputsAndReferenceInputs extends Ledger.STS.Validator[Ledger.StateI] {
        override def validate[StateT: StateI](
            context: Context,
            state: StateT,
            event: Event
        ): Either[Error, Unit] = {
            val inputs = event.body.inputs
            val referenceInputs = event.body.referenceInputs.getOrElse(Set.empty)
            val intersection = referenceInputs.view.filter(inputs.contains)

            if intersection.nonEmpty then
                return Left(
                  IllegalArgumentException(
                    s"Inputs and reference inputs intersects: intersection $intersection"
                  )
                );

            Right(())
        }
    }
}
