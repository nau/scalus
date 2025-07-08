package scalus.cardano.ledger
package rules

object FeeMutator extends STS.Mutator {
    override final type Error = Nothing | Throwable

    override def transit(context: Context, state: State, event: Event): Result = {
        context.fee += event.body.value.fee
        success(state)
    }
}
