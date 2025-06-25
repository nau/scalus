package scalus.cardano.ledger
package rules

import scalus.uplc.eval.{ExBudget, ExCPU, ExMemory}

// It's Alonzo.validateExUnitsTooBigUTxO in cardano ledger
object TransactionExecutionUnitsValidator extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionExUnits = calculateTransactionExUnits(event)
        val maxTxExecutionUnits = context.env.params.maxTxExecutionUnits
        if transactionExUnits.steps > maxTxExecutionUnits.steps ||
            transactionExUnits.memory > maxTxExecutionUnits.memory
        then
            return failure(
              IllegalArgumentException(
                s"Transaction execution units exceed the maximum allowed: " +
                    s"transaction: $transactionExUnits, " +
                    s"max allowed: $maxTxExecutionUnits"
              )
            )

        success
    }

    private def calculateTransactionExUnits(event: Event): ExUnits = {
        ???
    }
}
