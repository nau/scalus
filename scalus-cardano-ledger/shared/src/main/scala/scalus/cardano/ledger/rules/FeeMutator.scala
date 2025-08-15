package scalus.cardano.ledger
package rules

// WIP: Mutator that updates the fee in the context based on the transaction event
// Maybe will be deleted in the future, because it is not a part of Cardano ledger rules
object FeeMutator extends STS.Mutator {
    override final type Error = Nothing

    override def transit(context: Context, state: State, event: Event): Result = {
        context.fee += event.body.value.fee
        success(state)
    }
}
