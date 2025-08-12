package scalus.cardano.ledger.rules
import scalus.cardano.ledger.{ExUnits, TransactionException}

import scala.math.Ordered.orderingToOrdered

// Checks that the total execution units for a transaction don't exceed the protocol-defined maximum.
// It's Alonzo.validateExUnitsTooBigUTxO in cardano-ledger
object ExUnitsTooBigValidator extends STS.Validator {

    override type Error = TransactionException.ExUnitsExceedMaxException

    override def validate(
        context: Context,
        state: State,
        tx: Event
    ): ExUnitsTooBigValidator.Result = {
        val cap = context.env.params.maxTxExecutionUnits
        val totalExUnits = (for {
            redeemers <- tx.witnessSet.redeemers.toSeq
            redeemer <- redeemers.value.toSeq
            exUnit = redeemer.exUnits
        } yield exUnit).fold(ExUnits.zero)(_ + _)
        if totalExUnits > cap then {
            failure(
              TransactionException.ExUnitsExceedMaxException(
                tx.id,
                totalExUnits,
                cap
              )
            )
        } else success

    }

}
