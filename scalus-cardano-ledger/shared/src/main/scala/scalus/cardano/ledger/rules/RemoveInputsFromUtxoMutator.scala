package scalus.cardano.ledger
package rules

// It's part of Shelley.updateUTxOState in cardano-ledger
object RemoveInputsFromUtxoMutator extends STS.Mutator {
    override final type Error = Nothing

    override def transit(context: Context, state: State, event: Event): Result = {
        success(state.copy(utxo = state.utxo -- event.body.value.inputs))
    }
}
