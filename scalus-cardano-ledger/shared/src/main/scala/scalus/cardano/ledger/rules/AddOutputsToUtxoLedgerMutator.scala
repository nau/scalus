package scalus.cardano.ledger.rules

object AddOutputsToUtxoLedgerMutator extends Ledger.STS.Mutator[Ledger.StateI.Utxo]{
    override def transit[StateT: StateI](state: StateT, event: Event): Either[Error, StateT] = {
        val addedUtxo: Utxo = ???
        Right(state.utxo = state.utxo ++ addedUtxo)
    }
}
