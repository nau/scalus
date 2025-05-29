package scalus.cardano.ledger.rules

import scala.util.boundary
import scala.util.boundary.break

object MustBeInUtxoLedgerValidator {
    object Inputs extends LedgerValidator {
        override def validate(state: State, event: Event): Either[Error, Unit] = boundary {
            for input <- event.body.inputs
            do
                if !state.utxo.contains(input) then
                    break(Left(IllegalArgumentException(s"Unknown input $input")))

            Right(())
        }
    }

    object CollateralInputs extends LedgerValidator {
        override def validate(state: State, event: Event): Either[Error, Unit] = boundary {
            for collateralInput <- event.body.collateralInputs.getOrElse(Set.empty)
            do
                if !state.utxo.contains(collateralInput) then
                    break(
                      Left(IllegalArgumentException(s"Unknown collateral input $collateralInput"))
                    )

            Right(())
        }
    }

    object ReferenceInputs extends LedgerValidator {
        override def validate(state: State, event: Event): Either[Error, Unit] = boundary {
            for referenceInput <- event.body.referenceInputs.getOrElse(Set.empty)
            do
                if !state.utxo.contains(referenceInput) then
                    break(
                      Left(IllegalArgumentException(s"Unknown reference input $referenceInput"))
                    )

            Right(())
        }
    }

    object AllInputs extends LedgerValidator {
        override def validate(state: State, event: Event): Either[Error, Unit] = {
            for
                _ <- Inputs(state, event)
                _ <- CollateralInputs(state, event)
                _ <- ReferenceInputs(state, event)
            yield ()
        }
    }
}
