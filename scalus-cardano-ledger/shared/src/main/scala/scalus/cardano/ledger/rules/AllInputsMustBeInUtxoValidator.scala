package scalus.cardano.ledger.rules

import scala.util.boundary
import scala.util.boundary.break

// allInputs = spendInputs txb ∪ collInputs txb ∪ refInputs txb
// (spendInputs txb ∪ collInputs txb ∪ refInputs txb) ⊆ dom utxo
// It's Shelley.validateBadInputsUTxO in cardano-ledger
object AllInputsMustBeInUtxoValidator extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = boundary {
        val body = event.body.value
        for input <- body.inputs.view
                .concat(body.collateralInputs.getOrElse(Set.empty))
                .concat(body.referenceInputs.getOrElse(Set.empty))
        do
            if !state.utxo.contains(input) then
                break(failure(IllegalArgumentException(s"Unknown input $input")))

        success
    }
}
