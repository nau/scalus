package scalus.cardano.ledger
package rules

// It's part of Shelley.updateUTxOState in cardano-ledger
object AddOutputsToUtxoMutator extends STS.Mutator {
    override def transit(context: Context, state: State, event: Event): Result = {
        val addedUtxo: Utxo = event.body.value.outputs.view.zipWithIndex.map {
            case (Sized(output, _), index) =>
                TransactionInput(event.id, index) -> output
        }.toMap

        success(state.copy(utxo = state.utxo ++ addedUtxo))
    }
}
