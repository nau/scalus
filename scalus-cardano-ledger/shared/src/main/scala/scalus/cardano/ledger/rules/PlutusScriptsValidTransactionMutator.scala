package scalus.cardano.ledger
package rules

import scalus.ledger.api.MajorProtocolVersion
import scalus.uplc.eval.ExBudget
import scala.util.control.NonFatal

// It's conwayEvalScriptsTxValid in cardano-ledger
object PlutusScriptsValidTransactionMutator extends STS.Mutator {
    override final type Error = TransactionException.BadInputsUTxOException |
        TransactionException.BadReferenceInputsUTxOException |
        TransactionException.IllegalArgumentException

    override def transit(context: Context, state: State, event: Event): Result = {
        val body = event.body.value
        val slotConfig = context.slotConfig
        val protocolParameters = context.env.params
        val protocolVersion = protocolParameters.protocolVersion
        val costModels = protocolParameters.costModels
        val utxo = state.utxo

        try {
            PlutusScriptEvaluator(
              slotConfig = slotConfig,
              initialBudget = ExBudget.fromCpuAndMemory(10_000000000L, 10_000000L),
              protocolMajorVersion = MajorProtocolVersion(protocolVersion.major),
              costModels = CostModels.fromProtocolParams(protocolParameters),
              mode = EvaluatorMode.Validate,
              debugDumpFilesForTesting = false
            ).evalPlutusScripts(event, utxo)

            // TODO: implement state mutation
            success(state)
        } catch {
            // TODO: refine exception handling
            case NonFatal(exception) =>
                failure(
                  TransactionException.IllegalArgumentException(
                    s"Error during Plutus script evaluation: ${exception.getMessage}"
                  )
                )
        }
    }
}
