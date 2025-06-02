package scalus.cardano.ledger.rules

object FeeLedgerMutator extends Ledger.STS.Mutator[Ledger.StateI.Fee] {
    override def transit[StateT: StateI](
        context: Context,
        state: StateT,
        event: Event
    ): Either[Error, StateT] = {
        Right(state.fee += event.body.fee)
    }
}
