package scalus.bloxbean

import com.bloxbean.cardano.client.api.ProtocolParamsSupplier
import com.bloxbean.cardano.client.api.TransactionEvaluator
import com.bloxbean.cardano.client.api.UtxoSupplier
import com.bloxbean.cardano.client.api.exception.ApiException
import com.bloxbean.cardano.client.api.model.EvaluationResult
import com.bloxbean.cardano.client.api.model.Result
import com.bloxbean.cardano.client.api.model.Utxo
import com.bloxbean.cardano.client.api.util.CostModelUtil
import com.bloxbean.cardano.client.plutus.spec.*
import com.bloxbean.cardano.client.transaction.spec.Transaction
import com.bloxbean.cardano.client.transaction.spec.TransactionWitnessSet
import com.bloxbean.cardano.client.util.JsonUtil
import scalus.uplc.eval.ExBudget

import java.util
import java.util.ArrayList
import java.util.HashSet
import java.util.List
import java.util.Set
import scala.jdk.CollectionConverters.*

trait ScriptSupplier {
    def getScript(scriptHash: String): PlutusScript
}

/** Implements [[TransactionEvaluator]] to evaluate a transaction to get script costs using Scalus
  * evaluator. This is a wrapper around [[TxEvaluator]].
  */
class ScalusTransactionEvaluator(
    val utxoSupplier: UtxoSupplier,
    val protocolParamsSupplier: ProtocolParamsSupplier,
    val scriptSupplier: ScriptSupplier
) extends TransactionEvaluator {

    override def evaluateTx(
        cbor: Array[Byte],
        inputUtxos: util.Set[Utxo]
    ): Result[util.List[EvaluationResult]] = {
        try {
            val transaction = Transaction.deserialize(cbor)
            val utxos = new util.HashSet[Utxo]()
            // inputs//inputs

            for input <- transaction.getBody.getInputs.asScala do
                val utxo = utxoSupplier.getTxOutput(input.getTransactionId, input.getIndex).get
                utxos.add(utxo)

            val additionalScripts: util.List[PlutusScript] = new util.ArrayList[PlutusScript]
            // reference inputs//reference inputs

            for input <- transaction.getBody.getReferenceInputs.asScala do
                val utxo = utxoSupplier.getTxOutput(input.getTransactionId, input.getIndex).get
                utxos.add(utxo)
                // Get reference input script
                if utxo.getReferenceScriptHash != null && scriptSupplier != null then
                    additionalScripts.add(scriptSupplier.getScript(utxo.getReferenceScriptHash))

            if transaction.getWitnessSet == null then
                transaction.setWitnessSet(new TransactionWitnessSet)

            if transaction.getWitnessSet.getPlutusV1Scripts == null then
                transaction.getWitnessSet.setPlutusV1Scripts(new util.ArrayList)

            if transaction.getWitnessSet.getPlutusV2Scripts == null then
                transaction.getWitnessSet.setPlutusV2Scripts(new util.ArrayList)

            // FIXME: What??
            val language =
                if transaction.getWitnessSet != null && transaction.getWitnessSet.getPlutusV1Scripts != null && transaction.getWitnessSet.getPlutusV1Scripts.size > 0
                then Language.PLUTUS_V1
                else Language.PLUTUS_V2

            val protocolParams = protocolParamsSupplier.getProtocolParams
            val costModelOptional =
                CostModelUtil.getCostModelFromProtocolParams(protocolParams, language)

            // FIXME: What??
            if costModelOptional.isEmpty then
                throw ApiException("Cost model not found for language: " + language)

            val costMdls = CostMdls()
            costMdls.add(costModelOptional.get)

            val txBudget = ExBudget.fromCpuAndMemory(
              cpu = protocolParams.getMaxTxExSteps.toLong,
              memory = protocolParams.getMaxTxExMem.toLong
            )
            val txEvaluator = TxEvaluator(SlotConfig.default, txBudget)
            try
                val redeemers =
                    txEvaluator.evaluateTx(transaction, utxos, additionalScripts, costMdls)
                val evaluationResults = redeemers.stream.map { redeemer =>
                    EvaluationResult.builder
                        .redeemerTag(redeemer.getTag)
                        .index(redeemer.getIndex.intValue)
                        .exUnits(redeemer.getExUnits)
                        .build
                }.toList

                Result
                    .success(JsonUtil.getPrettyJson(evaluationResults))
                    .asInstanceOf[Result[util.List[EvaluationResult]]]
                    .withValue(evaluationResults)
                    .asInstanceOf[Result[util.List[EvaluationResult]]]
            catch
                case e: Exception =>
                    Result
                        .error(s"Error evaluating transaction: ${e.getMessage}")
                        .asInstanceOf[Result[util.List[EvaluationResult]]]
        } catch {
            case e: Exception => throw ApiException("Error evaluating transaction", e)
        }
    }
}
