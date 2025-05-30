package scalus.cardano.ledger.rules

object RemoveInputsFromUtxoLedgerMutator extends Ledger.STS.Mutator[Ledger.StateI.Utxo]{
    override def transit[StateT: StateI](state: StateT, event: Event): Either[Error, StateT] = {
        Right(state.utxo = state.utxo -- event.body.inputs)
    }
}
