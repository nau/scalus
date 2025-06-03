package scalus.cardano.ledger.rules

object FeeMutator extends STS.Mutator {
    override def transit(context: Context, state: State, event: Event): Result = {
        context.fee += event.body.fee
        success(state)
    }
}
