package scalus.cardano.ledger.rules

import scalus.cardano.ledger.TransactionException.WrongNetworkWithdrawal

// It's part of Shelley.validateWrongNetworkWithdrawal in cardano-ledger
object WrongNetworkWithdrawalValidator extends STS.Validator {
    override final type Error = WrongNetworkWithdrawal

    override def validate(context: Context, state: State, event: Event): Result = {
        (for
            ws <- event.body.value.withdrawals
            w <- ws.withdrawals.find(_._1.address.network != context.env.network)
        yield failure(WrongNetworkWithdrawal(event.id, w._1.address.network)))
            .getOrElse(success)
    }
}
