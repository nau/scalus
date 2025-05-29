package scalus.cardano.ledger.rules

// It's part of Shelley.updateUTxOState in cardano-ledger
object FeeMutator extends STS.Mutator {
    override def transit(context: Context, state: State, event: Event): Result = {
        context.fee += event.body.value.fee
        success(state)
    }
}
