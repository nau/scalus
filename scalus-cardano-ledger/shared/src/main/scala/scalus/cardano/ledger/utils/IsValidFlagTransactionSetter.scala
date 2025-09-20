package scalus.cardano.ledger
package utils

import scalus.uplc.eval.ExBudget

object IsValidFlagTransactionSetter {
    def setIsValidTransactionFlag(
        transaction: Transaction,
        utxo: UTxO,
        slotConfig: SlotConfig,
        protocolParameters: ProtocolParams
    ): Transaction = {
        val maxTxExecutionUnits = protocolParameters.maxTxExecutionUnits
        val protocolVersion = protocolParameters.protocolVersion
        val costModels = protocolParameters.costModels

        try {
            PlutusScriptEvaluator(
              slotConfig = slotConfig,
              initialBudget =
                  ExBudget.fromCpuAndMemory(maxTxExecutionUnits.steps, maxTxExecutionUnits.memory),
              protocolMajorVersion = MajorProtocolVersion(protocolVersion.major),
              costModels = CostModels.fromProtocolParams(protocolParameters),
              mode = EvaluatorMode.Validate,
              debugDumpFilesForTesting = false
            ).evalPlutusScripts(transaction, utxo)

            transaction.copy(isValid = true)
        } catch {
            case e: PlutusScriptEvaluationException => transaction.copy(isValid = false)
        }
    }
}
