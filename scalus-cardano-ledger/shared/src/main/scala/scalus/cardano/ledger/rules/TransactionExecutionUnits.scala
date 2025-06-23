package scalus.cardano.ledger
package rules

import scalus.uplc.eval.{ExBudget, ExCPU, ExMemory}

// It's Alonzo.validateExUnitsTooBigUTxO in cardano ledger
object TransactionExecutionUnits extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionExBudget = calculateTransactionExBudget(event)
        val maxTransactionExBudget = {
            val maxTxExecutionUnits = context.env.params.maxTxExecutionUnits
            ExBudget(ExCPU(maxTxExecutionUnits.steps), ExMemory(maxTxExecutionUnits.memory))
        }
        if transactionExBudget.cpu > maxTransactionExBudget.cpu ||
            transactionExBudget.memory > maxTransactionExBudget.memory
        then
            return failure(
              IllegalArgumentException(
                s"Transaction execution units exceed the maximum allowed: " +
                    s"transaction: ${transactionExBudget.showJson}, " +
                    s"max allowed: ${maxTransactionExBudget.showJson}"
              )
            )

        success
    }

    private def calculateTransactionExBudget(event: Event): ExBudget = {
        ???
    }
}
