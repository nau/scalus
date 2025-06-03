package scalus.cardano.ledger.rules

import scala.util.boundary
import scala.util.boundary.break

object AllInputsMustBeInUtxoValidator extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = boundary {
        for input <- event.body.inputs.view
                .concat(event.body.collateralInputs.getOrElse(Set.empty))
                .concat(event.body.referenceInputs.getOrElse(Set.empty))
        do
            if !state.utxo.contains(input) then
                break(failure(IllegalArgumentException(s"Unknown input $input")))

        success
    }
}
