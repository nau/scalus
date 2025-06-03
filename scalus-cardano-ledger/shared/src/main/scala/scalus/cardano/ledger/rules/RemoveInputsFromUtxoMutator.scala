package scalus.cardano.ledger.rules

object RemoveInputsFromUtxoMutator extends STS.Mutator {
    override def transit(context: Context, state: State, event: Event): Result = {
        success(state.copy(utxo = state.utxo -- event.body.inputs))
    }
}
