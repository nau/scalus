package scalus.cardano.ledger.rules

import scalus.cardano.ledger.TransactionException.WrongNetworkAddress

// It's part of Shelley.validateWrongNetwork in cardano-ledger
object WrongNetworkValidator extends STS.Validator {
    override final type Error = WrongNetworkAddress

    override def validate(context: Context, state: State, event: Event): Result = {
        event.body.value.outputs
            .find(_.value.address.getNetwork.fold(false)(_ != context.env.network))
            .map(t => failure(WrongNetworkAddress(event.id, t.value.address)))
            .getOrElse(success)
    }
}
