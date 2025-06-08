package scalus.cardano.ledger
package rules

import scala.util.boundary
import scala.util.boundary.break

// allInputs = spendInputs txb ∪ collInputs txb ∪ refInputs txb
// (spendInputs txb ∪ collInputs txb ∪ refInputs txb) ⊆ dom utxo
// It's Shelley.validateBadInputsUTxO in cardano-ledger
object AllInputsMustBeInUtxoValidator extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = {
        for
            _ <- validateInputs(context, state, event)
            _ <- validateCollateralInputs(context, state, event)
            _ <- validateReferenceInputs(context, state, event)
        yield ()
    }

    private[this] def validateInputs(context: Context, state: State, event: Event): Result =
        validate(
          event.id,
          event.body.value.inputs,
          state.utxo,
          (transactionId, input, index) =>
              IllegalArgumentException(
                s"Missing input $input with index $index in UTxO state for transactionId $transactionId"
              )
        )

    private[this] def validateCollateralInputs(
        context: Context,
        state: State,
        event: Event
    ): Result =
        validate(
          event.id,
          event.body.value.collateralInputs,
          state.utxo,
          (transactionId, collateralInput, index) =>
              IllegalArgumentException(
                s"Missing collateralInput $collateralInput with index $index in UTxO state for transactionId $transactionId"
              )
        )

    private[this] def validateReferenceInputs(
        context: Context,
        state: State,
        event: Event
    ): Result =
        validate(
          event.id,
          event.body.value.referenceInputs,
          state.utxo,
          (transactionId, referenceInputs, index) =>
              IllegalArgumentException(
                s"Missing referenceInputs $referenceInputs with index $index in UTxO state for transactionId $transactionId"
              )
        )

    private[this] def validate(
        transactionId: TransactionHash,
        inputs: Set[TransactionInput],
        utxo: Utxo,
        error: (TransactionHash, TransactionInput, Int) => IllegalArgumentException
    ): Result =
        boundary {
            for (input, index) <- inputs.view.zipWithIndex
            do if !utxo.contains(input) then break(failure(error(transactionId, input, index)))

            success
        }
}
