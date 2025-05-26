package scalus.cardano.ledger.rules

import scala.util.boundary
import scala.util.boundary.break

object BadInputsUtxOLedgerSTM extends LedgerSTM {
    override def transit(state: State, event: Event): Either[Error, State] = boundary {
        val spentInputs = event.body.inputs
        val collateralInputs = event.body.collateralInputs.getOrElse(Set.empty)
        val referenceInputs = event.body.referenceInputs.getOrElse(Set.empty)

        for input <- spentInputs.view.concat(collateralInputs).concat(referenceInputs)
        do
            if !state.utxo.contains(input) then
                break(Left(IllegalArgumentException(s"Unknown input $input")))

        Right(state)
    }
}
