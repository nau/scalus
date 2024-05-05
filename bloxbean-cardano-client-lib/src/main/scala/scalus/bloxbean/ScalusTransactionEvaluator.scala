package scalus.bloxbean;

import com.bloxbean.cardano.client.api.ProtocolParamsSupplier;
import com.bloxbean.cardano.client.api.TransactionEvaluator;
import com.bloxbean.cardano.client.api.UtxoSupplier;
import com.bloxbean.cardano.client.api.exception.ApiException;
import com.bloxbean.cardano.client.api.model.EvaluationResult;
import com.bloxbean.cardano.client.api.model.ProtocolParams;
import com.bloxbean.cardano.client.api.model.Result;
import com.bloxbean.cardano.client.api.model.Utxo;
import com.bloxbean.cardano.client.api.util.CostModelUtil;
import com.bloxbean.cardano.client.backend.api.BackendService;
import com.bloxbean.cardano.client.backend.api.DefaultProtocolParamsSupplier;
import com.bloxbean.cardano.client.backend.api.DefaultUtxoSupplier;
import com.bloxbean.cardano.client.plutus.spec.*;
import com.bloxbean.cardano.client.transaction.spec.Transaction;
import com.bloxbean.cardano.client.transaction.spec.TransactionInput;
import com.bloxbean.cardano.client.transaction.spec.TransactionWitnessSet;
import com.bloxbean.cardano.client.util.JsonUtil;

import java.util.*;
import java.util.stream.Collectors;

/** Implements TransactionEvaluator to evaluate a transaction to get script costs using Aiken
  * evaluator. This is a wrapper around TxEvaluator.
  */
import scala.jdk.CollectionConverters.*
import scalus.uplc.eval.ExBudget
import scalus.uplc.eval.MemoryUsage.memoryUsage
import scala.collection.mutable

class ScalusTransactionEvaluator(
    val utxoSupplier: UtxoSupplier,
    val protocolParamsSupplier: ProtocolParamsSupplier
) extends TransactionEvaluator:

    override def evaluateTx(
        cbor: Array[Byte],
        inputUtxos: Set[Utxo]
    ): Result[List[EvaluationResult]] =
        try
            val transaction = Transaction.deserialize(cbor)

            val utxos = transaction.getBody.getInputs
                .stream()
                .map { input =>
                    utxoSupplier.getTxOutput(input.getTransactionId, input.getIndex).get
                }
                .collect(Collectors.toSet[Utxo])
            val additionalScripts = new ArrayList[PlutusScript]()

            if transaction.getWitnessSet == null then
                transaction.setWitnessSet(new TransactionWitnessSet)

            if transaction.getWitnessSet.getPlutusV1Scripts == null then
                transaction.getWitnessSet.setPlutusV1Scripts(new ArrayList)

            if transaction.getWitnessSet.getPlutusV2Scripts == null then
                transaction.getWitnessSet.setPlutusV2Scripts(new ArrayList)

            val language =
                if transaction.getWitnessSet != null && transaction.getWitnessSet.getPlutusV1Scripts != null && transaction.getWitnessSet.getPlutusV1Scripts.size > 0
                then Language.PLUTUS_V1
                else Language.PLUTUS_V2

            val protocolParams = protocolParamsSupplier.getProtocolParams
            val costModelOptional =
                CostModelUtil.getCostModelFromProtocolParams(protocolParams, language)
            if costModelOptional.isEmpty then
                throw ApiException("Cost model not found for language: " + language)

            val costMdls = CostMdls()
            costMdls.add(costModelOptional.get)

            val cpu = protocolParams.getMaxTxExSteps().toLong
            val memoryUsage = protocolParams.getMaxTxExMem().toLong
            val txEvaluator =
                TxEvaluator(SlotConfig.default, ExBudget.fromCpuAndMemory(cpu, memoryUsage))
            val redeemers = txEvaluator.evaluateTx(transaction, utxos, additionalScripts, costMdls)
            if redeemers == null then
                return Result
                    .error("Error evaluating transaction")
                    .asInstanceOf[Result[List[EvaluationResult]]]

            val evaluationResults = redeemers.asScala.map { redeemer =>
                EvaluationResult.builder
                    .redeemerTag(redeemer.getTag)
                    .index(redeemer.getIndex.intValue)
                    .exUnits(redeemer.getExUnits)
                    .build
            }.asJava

            Result
                .success(JsonUtil.getPrettyJson(evaluationResults))
                .asInstanceOf[Result[List[EvaluationResult]]]
                .withValue(evaluationResults)
                .asInstanceOf[Result[List[EvaluationResult]]]
        catch case e: Exception => throw ApiException("Error evaluating transaction", e)
