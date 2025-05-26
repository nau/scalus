package scalus.cardano.ledger.rules

object InputSetEmptyUtxoLedgerSTM extends LedgerSTM {
    override def transit(state: State, event: Event): Either[Error, State] = {
        if event.body.inputs.isEmpty then
            return Left(IllegalArgumentException("Empty transaction spent inputs"))

        Right(state)
    }
}
