package scalus.cardano.ledger.rules

object InputsLedgerSTM extends LedgerSTM {
    override def transit(state: State, event: Event): Either[Error, State] = {
        for
            state <- InputSetEmptyUtxoLedgerSTM(state, event)
            state <- DisjointRefInputsLedgerSTM(state, event)
            state <- BadInputsUtxOLedgerSTM(state, event)
        yield state
    }
}
