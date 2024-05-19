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
    var txEvaluator: TxEvaluator = null

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

            val ins = co.nstant.in.cbor.model.Array()
            val outs = co.nstant.in.cbor.model.Array()
            for utxo <- utxos.asScala.toSeq do
                ins.add(
                  TransactionInput
                      .builder()
                      .index(utxo.getOutputIndex)
                      .transactionId(utxo.getTxHash)
                      .build()
                      .serialize()
                )
                outs.add(
                  TransactionOutput
                      .builder()
                      .value(utxo.toValue)
                      .address(utxo.getAddress)
                      .datumHash(Option(utxo.getDataHash).map(Utils.hexToBytes).orNull)
                      .build()
                      .serialize()
                )

//            Files.write(java.nio.file.Path.of("ins.cbor"), CborSerializationUtil.serialize(ins));
//            Files.write(java.nio.file.Path.of("outs.cbor"), CborSerializationUtil.serialize(outs));

            val additionalScripts: util.List[PlutusScript] = new util.ArrayList[PlutusScript]
            // reference inputs//reference inputs

            for input <- transaction.getBody.getReferenceInputs.asScala do
                val utxo = utxoSupplier.getTxOutput(input.getTransactionId, input.getIndex).get
                utxos.add(utxo)
                // Get reference input script
//                println(s"Reference input: ${utxo.getReferenceScriptHash}")
                if utxo.getReferenceScriptHash != null && scriptSupplier != null then
                    additionalScripts.add(scriptSupplier.getScript(utxo.getReferenceScriptHash))

            if transaction.getWitnessSet == null then
                transaction.setWitnessSet(new TransactionWitnessSet)

            if transaction.getWitnessSet.getPlutusV1Scripts == null then
                transaction.getWitnessSet.setPlutusV1Scripts(new util.ArrayList)

            if transaction.getWitnessSet.getPlutusV2Scripts == null then
                transaction.getWitnessSet.setPlutusV2Scripts(new util.ArrayList)

            if transaction.getWitnessSet.getPlutusDataList == null then
                transaction.getWitnessSet.setPlutusDataList(new util.ArrayList)

            val protocolParams = protocolParamsSupplier.getProtocolParams
            val costModelV1 = CostModelUtil
                .getCostModelFromProtocolParams(protocolParams, Language.PLUTUS_V1)
                .get()
            val costModelV2 = CostModelUtil
                .getCostModelFromProtocolParams(protocolParams, Language.PLUTUS_V2)
                .get()

            val costMdls = CostMdls()
            costMdls.add(costModelV1)
            costMdls.add(costModelV2)

            val txBudget = ExBudget.fromCpuAndMemory(
              cpu = protocolParams.getMaxTxExSteps.toLong,
              memory = protocolParams.getMaxTxExMem.toLong
            )
            txEvaluator = TxEvaluator(SlotConfig.default, txBudget)
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
