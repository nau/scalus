package scalus.cardano.ledger.rules

import scala.util.boundary
import scala.util.boundary.break

object MustBeInUtxoLedgerValidator {
    object Inputs extends Ledger.STS.Validator[Ledger.StateI.Utxo] {
        override def validate[StateT: StateI](
            context: Context,
            state: StateT,
            event: Event
        ): Either[Error, Unit] = boundary {
            for input <- event.body.inputs
            do
                if !state.utxo.contains(input) then
                    break(Left(IllegalArgumentException(s"Unknown input $input")))

            Right(())
        }
    }

    object CollateralInputs extends Ledger.STS.Validator[Ledger.StateI.Utxo] {
        override def validate[StateT: StateI](
            context: Context,
            state: StateT,
            event: Event
        ): Either[Error, Unit] = boundary {
            for collateralInput <- event.body.collateralInputs.getOrElse(Set.empty)
            do
                if !state.utxo.contains(collateralInput) then
                    break(
                      Left(IllegalArgumentException(s"Unknown collateral input $collateralInput"))
                    )

            Right(())
        }
    }

    object ReferenceInputs extends Ledger.STS.Validator[Ledger.StateI.Utxo] {
        override def validate[StateT: StateI](
            context: Context,
            state: StateT,
            event: Event
        ): Either[Error, Unit] = boundary {
            for referenceInput <- event.body.referenceInputs.getOrElse(Set.empty)
            do
                if !state.utxo.contains(referenceInput) then
                    break(
                      Left(IllegalArgumentException(s"Unknown reference input $referenceInput"))
                    )

            Right(())
        }
    }

    object AllInputs extends Ledger.STS.Validator[Ledger.StateI.Utxo] {
        override def validate[StateT: StateI](
            context: Context,
            state: StateT,
            event: Event
        ): Either[Error, Unit] = {
            for
                _ <- Inputs.validate(context, state, event)
                _ <- CollateralInputs.validate(context, state, event)
                _ <- ReferenceInputs.validate(context, state, event)
            yield ()
        }
    }
}
