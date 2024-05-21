package scalus.bloxbean

import co.nstant.in.cbor.model
import com.bloxbean.cardano.client.api.ProtocolParamsSupplier
import com.bloxbean.cardano.client.api.TransactionEvaluator
import com.bloxbean.cardano.client.api.UtxoSupplier
import com.bloxbean.cardano.client.api.exception.ApiException
import com.bloxbean.cardano.client.api.model.EvaluationResult
import com.bloxbean.cardano.client.api.model.ProtocolParams
import com.bloxbean.cardano.client.api.model.Result
import com.bloxbean.cardano.client.api.model.Utxo
import com.bloxbean.cardano.client.api.util.CostModelUtil
import com.bloxbean.cardano.client.common.cbor.CborSerializationUtil
import com.bloxbean.cardano.client.exception.CborDeserializationException
import com.bloxbean.cardano.client.exception.CborSerializationException
import com.bloxbean.cardano.client.plutus.spec.*
import com.bloxbean.cardano.client.spec.Script
import com.bloxbean.cardano.client.transaction.spec.Transaction
import com.bloxbean.cardano.client.transaction.spec.TransactionInput
import com.bloxbean.cardano.client.transaction.spec.TransactionOutput
import com.bloxbean.cardano.client.transaction.spec.TransactionWitnessSet
import com.bloxbean.cardano.client.util.JsonUtil
import scalus.uplc.eval.ExBudget
import scalus.utils.Utils

import java.nio.file.Files
import java.util
import java.util.ArrayList
import java.util.HashSet
import java.util.List
import java.util.Set
import java.util.stream.Collectors
import scala.beans.BeanProperty
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
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
    @BeanProperty
    lazy val protocolParams: ProtocolParams = protocolParamsSupplier.getProtocolParams

    @BeanProperty
    lazy val costMdls: CostMdls = {
        val costModelV1 = CostModelUtil
            .getCostModelFromProtocolParams(protocolParams, Language.PLUTUS_V1)
            .get()
        val costModelV2 = CostModelUtil
            .getCostModelFromProtocolParams(protocolParams, Language.PLUTUS_V2)
            .get()
        val cm = CostMdls()
        cm.add(costModelV1)
        cm.add(costModelV2)
        cm
    }

    // Initialize tx evaluator
    private lazy val txEvaluator =
        val txBudget = ExBudget.fromCpuAndMemory(
          cpu = protocolParams.getMaxTxExSteps.toLong,
          memory = protocolParams.getMaxTxExMem.toLong
        )

        TxEvaluator(
          SlotConfig.default,
          txBudget,
          protocolParams.getProtocolMajorVer.intValue(),
          costMdls,
          failOnBudgetMismatch = false
        )

    override def evaluateTx(
        transaction: Transaction,
        inputUtxos: util.Set[Utxo],
    ): Result[util.List[EvaluationResult]] = {
        evaluateTx(transaction, inputUtxos, new util.ArrayList())
    }

    def evaluateTx(
        transaction: Transaction,
        inputUtxos: util.Set[Utxo],
        originalDatumHashes: util.List[scalus.builtin.ByteString]
    ): Result[util.List[EvaluationResult]] = {
        try {
            // initialize utxos with inputUtxos
            val utxos =
                val utxos = new mutable.HashMap[TransactionInput, Utxo]()
                for utxo <- inputUtxos.asScala do
                    val input = TransactionInput.builder
                        .transactionId(utxo.getTxHash)
                        .index(utxo.getOutputIndex)
                        .build
                    utxos.put(input, utxo)
                utxos

            // Get all input utxos
            for input <- transaction.getBody.getInputs.asScala do
                if !utxos.contains(input) then
                    val utxo = utxoSupplier.getTxOutput(input.getTransactionId, input.getIndex).get
                    utxos.put(input, utxo)

            val scripts = new mutable.HashMap[String, Script]()

            // Get all reference inputs utxos including scripts
            for input <- transaction.getBody.getReferenceInputs.asScala do
                if !utxos.contains(input) then
                    val utxo = utxoSupplier.getTxOutput(input.getTransactionId, input.getIndex).get
                    utxos.put(input, utxo)
                    // Get reference input script
                    if utxo.getReferenceScriptHash != null && scriptSupplier != null then
                        val script = scriptSupplier.getScript(utxo.getReferenceScriptHash)
                        scripts.put(utxo.getReferenceScriptHash, script)

            // Initialize witness set to avoid null pointer exceptions
            if transaction.getWitnessSet == null then
                transaction.setWitnessSet(new TransactionWitnessSet)

            val witnessSet = transaction.getWitnessSet
            if witnessSet.getNativeScripts == null then
                witnessSet.setNativeScripts(new util.ArrayList)

            if witnessSet.getPlutusV1Scripts == null then
                witnessSet.setPlutusV1Scripts(new util.ArrayList)

            if witnessSet.getPlutusV2Scripts == null then
                witnessSet.setPlutusV2Scripts(new util.ArrayList)

            if witnessSet.getPlutusDataList == null then
                witnessSet.setPlutusDataList(new util.ArrayList)

            // Resolve Utxos
            val resolvedUtxos =
                val witnessScripts = transaction.getWitnessSet.getPlutusV1Scripts.asScala
                    ++ transaction.getWitnessSet.getPlutusV2Scripts.asScala
                    ++ transaction.getWitnessSet.getNativeScripts.asScala

                for s <- witnessScripts do scripts.put(Utils.bytesToHex(s.getScriptHash), s)

                val allInputs =
                    transaction.getBody.getInputs.asScala ++
                        transaction.getBody.getReferenceInputs.asScala

                resolveTxInputs(allInputs, utxos, scripts)

            try
                val redeemers = txEvaluator.evaluateTx(transaction, resolvedUtxos, originalDatumHashes.asScala)
                val evaluationResults = redeemers.map { redeemer =>
                    EvaluationResult.builder
                        .redeemerTag(redeemer.getTag)
                        .index(redeemer.getIndex.intValue)
                        .exUnits(redeemer.getExUnits)
                        .build
                }.asJava

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

    override def evaluateTx(
        cbor: Array[Byte],
        inputUtxos: util.Set[Utxo]
    ): Result[util.List[EvaluationResult]] = {
        evaluateTx(Transaction.deserialize(cbor), inputUtxos)
    }

    private def resolveTxInputs(
        inputs: collection.Seq[TransactionInput],
        utxos: collection.Map[TransactionInput, Utxo],
        plutusScripts: collection.Map[String, Script]
    ): Map[TransactionInput, TransactionOutput] = {
        inputs.map { input =>
            val utxo = utxos.getOrElse(input, throw new IllegalStateException())
            val scriptOpt = plutusScripts.get(utxo.getReferenceScriptHash)
            val inlineDatum = Option(utxo.getInlineDatum).map(hex =>
                PlutusData.deserialize(Utils.hexToBytes(hex))
            )
            val datumHash = Option(utxo.getDataHash).map(Utils.hexToBytes)
            try
                input -> TransactionOutput.builder
                    .address(utxo.getAddress)
                    .value(utxo.toValue)
                    .datumHash(if inlineDatum.isEmpty then datumHash.orNull else null)
                    .inlineDatum(inlineDatum.orNull)
                    .scriptRef(scriptOpt.orNull)
                    .build
            catch case e: CborDeserializationException => throw new IllegalStateException(e)
        }.toMap
    }
}
