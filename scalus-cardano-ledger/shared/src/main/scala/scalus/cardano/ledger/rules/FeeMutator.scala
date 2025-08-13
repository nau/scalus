package scalus.cardano.ledger
package rules

// FIXME: Is it a part of Shelley.updateUTxOState in cardano-ledger?
object FeeMutator extends STS.Mutator {
    override final type Error = Nothing

    override def transit(context: Context, state: State, event: Event): Result = {
        context.fee += event.body.value.fee
        success(state)
    }
}
