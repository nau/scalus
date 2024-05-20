package scalus.bloxbean

import com.bloxbean.cardano.client.api.ProtocolParamsSupplier
import com.bloxbean.cardano.client.api.TransactionEvaluator
import com.bloxbean.cardano.client.api.UtxoSupplier
import com.bloxbean.cardano.client.api.exception.ApiException
import com.bloxbean.cardano.client.api.model.EvaluationResult
import com.bloxbean.cardano.client.api.model.Result
import com.bloxbean.cardano.client.api.model.Utxo
import com.bloxbean.cardano.client.api.util.CostModelUtil
import com.bloxbean.cardano.client.common.cbor.CborSerializationUtil
import com.bloxbean.cardano.client.exception.CborDeserializationException
import com.bloxbean.cardano.client.exception.CborSerializationException
import com.bloxbean.cardano.client.plutus.spec.*
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
    lazy val protocolParams = protocolParamsSupplier.getProtocolParams

    lazy val costMdls = {
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

    override def evaluateTx(
        transaction: Transaction,
        inputUtxos: util.Set[Utxo]
    ): Result[util.List[EvaluationResult]] = {
        try {
            val utxos = new util.HashSet[Utxo]()
            // inputs//inputs

            for input <- transaction.getBody.getInputs.asScala do
                val utxo = utxoSupplier.getTxOutput(input.getTransactionId, input.getIndex).get
                utxos.add(utxo)

            val ins = co.nstant.in.cbor.model.Array()
            val outs = co.nstant.in.cbor.model.Array()

            val additionalScripts: util.List[PlutusScript] = new util.ArrayList[PlutusScript]
            val scripts: util.Map[String, PlutusScript] = new util.HashMap[String, PlutusScript]()
            // reference inputs//reference inputs

            for input <- transaction.getBody.getReferenceInputs.asScala do
                val utxo = utxoSupplier.getTxOutput(input.getTransactionId, input.getIndex).get
                utxos.add(utxo)
                // Get reference input script
                //                println(s"Reference input: ${resolvedUtxos.getReferenceScriptHash}")
                if utxo.getReferenceScriptHash != null && scriptSupplier != null then
                    val script = scriptSupplier.getScript(utxo.getReferenceScriptHash)
                    additionalScripts.add(script)
                    scripts.put(utxo.getReferenceScriptHash, script)

            for utxo <- utxos.asScala.toSeq do
                ins.add(
                  TransactionInput
                      .builder()
                      .index(utxo.getOutputIndex)
                      .transactionId(utxo.getTxHash)
                      .build()
                      .serialize()
                )
                val inlineDatum = Option(utxo.getInlineDatum).map(hex =>
                    PlutusData.deserialize(Utils.hexToBytes(hex))
                )
                val datumHash = Option(utxo.getDataHash).map(Utils.hexToBytes)
                outs.add(
                  TransactionOutput
                      .builder()
                      .value(utxo.toValue)
                      .address(utxo.getAddress)
                      .inlineDatum(inlineDatum.orNull)
                      .datumHash(if inlineDatum.isEmpty then datumHash.orNull else null)
                      .scriptRef(Option(scripts.get(utxo.getReferenceScriptHash)).orNull)
                      .build()
                      .serialize()
                )

            //            Files.write(java.nio.file.Path.of("ins.cbor"), CborSerializationUtil.serialize(ins));
            //            Files.write(java.nio.file.Path.of("outs.cbor"), CborSerializationUtil.serialize(outs));

            if transaction.getWitnessSet == null then
                transaction.setWitnessSet(new TransactionWitnessSet)

            if transaction.getWitnessSet.getNativeScripts == null then
                transaction.getWitnessSet.setNativeScripts(new util.ArrayList)

            if transaction.getWitnessSet.getPlutusV1Scripts == null then
                transaction.getWitnessSet.setPlutusV1Scripts(new util.ArrayList)

            if transaction.getWitnessSet.getPlutusV2Scripts == null then
                transaction.getWitnessSet.setPlutusV2Scripts(new util.ArrayList)

            if transaction.getWitnessSet.getPlutusDataList == null then
                transaction.getWitnessSet.setPlutusDataList(new util.ArrayList)

            val witnessScripts = new util.ArrayList[PlutusScript]
            witnessScripts.addAll(transaction.getWitnessSet.getPlutusV1Scripts)
            witnessScripts.addAll(transaction.getWitnessSet.getPlutusV2Scripts)

            val allScripts =
                util.stream.Stream
                    .concat(additionalScripts.stream, witnessScripts.stream)
                    .collect(Collectors.toList)

            val txInputs = transaction.getBody.getInputs
            val refTxInputs = transaction.getBody.getReferenceInputs
            val allInputs =
                util.stream.Stream
                    .concat(txInputs.stream, refTxInputs.stream)
                    .collect(Collectors.toList)
            val txOutputs = resolveTxInputs(allInputs, utxos, allScripts)
            val resolvedUtxos = allInputs.asScala
                .zip(txOutputs.asScala)
                .toMap

            val txEvaluator =
                val txBudget = ExBudget.fromCpuAndMemory(
                  cpu = protocolParams.getMaxTxExSteps.toLong,
                  memory = protocolParams.getMaxTxExMem.toLong
                )
                
                TxEvaluator(
                  SlotConfig.default,
                  txBudget,
                  protocolParams.getProtocolMajorVer.intValue(),
                  costMdls
                )
            try
                val redeemers =
                    txEvaluator.evaluateTx(transaction, resolvedUtxos)
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

    override def evaluateTx(
        cbor: Array[Byte],
        inputUtxos: util.Set[Utxo]
    ): Result[util.List[EvaluationResult]] = {
        evaluateTx(Transaction.deserialize(cbor), inputUtxos)
    }

    private def resolveTxInputs(
        inputs: util.List[TransactionInput],
        utxos: util.Set[Utxo],
        plutusScripts: util.List[PlutusScript]
    ): util.List[TransactionOutput] = {
        inputs.stream
            .map { input =>
                val utxo = utxos.asScala
                    .find(utxo =>
                        input.getTransactionId == utxo.getTxHash && input.getIndex == utxo.getOutputIndex
                    )
                    .getOrElse(throw new IllegalStateException())

                val address = utxo.getAddress

                val plutusScript = plutusScripts.asScala.find { script =>
                    try Utils.bytesToHex(script.getScriptHash) == utxo.getReferenceScriptHash
                    catch case e: CborSerializationException => throw new IllegalStateException(e)
                }

                val inlineDatum = Option(utxo.getInlineDatum).map(hex =>
                    PlutusData.deserialize(Utils.hexToBytes(hex))
                )
                val datumHash = Option(utxo.getDataHash).map(Utils.hexToBytes)
                //                println(s"datumHash: $datumHash, inlineDatum: $inlineDatum, utxo: $utxo")

                try
                    TransactionOutput.builder
                        .address(address)
                        .value(utxo.toValue)
                        .datumHash(if inlineDatum.isEmpty then datumHash.orNull else null)
                        .inlineDatum(inlineDatum.orNull)
                        .scriptRef(plutusScript.orNull)
                        .build
                catch case e: CborDeserializationException => throw new IllegalStateException(e)
            }
            .collect(Collectors.toList)
    }
}
