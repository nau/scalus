package scalus.bloxbean

import com.bloxbean.cardano.client.api.TransactionEvaluator
import com.bloxbean.cardano.client.api.UtxoSupplier
import com.bloxbean.cardano.client.api.exception.ApiException
import com.bloxbean.cardano.client.api.model.EvaluationResult
import com.bloxbean.cardano.client.api.model.ProtocolParams
import com.bloxbean.cardano.client.api.model.Result
import com.bloxbean.cardano.client.api.model.Utxo
import com.bloxbean.cardano.client.api.util.CostModelUtil
import com.bloxbean.cardano.client.exception.CborDeserializationException
import com.bloxbean.cardano.client.plutus.spec.*
import com.bloxbean.cardano.client.spec.Script
import com.bloxbean.cardano.client.transaction.spec.Transaction
import com.bloxbean.cardano.client.transaction.spec.TransactionInput
import com.bloxbean.cardano.client.transaction.spec.TransactionOutput
import com.bloxbean.cardano.client.transaction.spec.TransactionWitnessSet
import com.bloxbean.cardano.client.util.JsonUtil
import scalus.builtin.ByteString
import scalus.uplc.eval.ExBudget
import scalus.utils.Utils

import java.util
import java.util.ArrayList
import java.util.List
import java.util.Set
import scala.beans.BeanProperty
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** Implements [[TransactionEvaluator]] to evaluate a transaction to get script costs using Scalus
  * evaluator. This is a wrapper around [[TxEvaluator]].
  * @param slotConfig
  *   Slot configuration
  * @param protocolParams
  *   Protocol parameters
  * @param utxoSupplier
  *   Utxo supplier
  * @param scriptSupplier
  *   Additional script supplier
  * @param mode
  *   Evaluator mode.
  *   - [[EvaluatorMode.EVALUATE_AND_COMPUTE_COST]] will evaluate the transaction and compute the
  *     cost
  *   - [[EvaluatorMode.VALIDATE]] will validate the transaction and fail if execution budget
  *     exceeds
  */
class ScalusTransactionEvaluator(
    @BeanProperty val slotConfig: SlotConfig,
    @BeanProperty val protocolParams: ProtocolParams,
    @BeanProperty val utxoSupplier: UtxoSupplier,
    @BeanProperty val scriptSupplier: ScriptSupplier,
    @BeanProperty val mode: EvaluatorMode
) extends TransactionEvaluator {

    /** Constructor with protocol params, utxo supplier, script supplier and mode. Uses
      * [[SlotConfig.Mainnet]].
      *
      * @param protocolParams
      * @param utxoSupplier
      * @param scriptSupplier
      * @param mode
      */
    def this(
        protocolParams: ProtocolParams,
        utxoSupplier: UtxoSupplier,
        scriptSupplier: ScriptSupplier,
        mode: EvaluatorMode
    ) = this(SlotConfig.Mainnet, protocolParams, utxoSupplier, scriptSupplier, mode)

    /** Constructor with protocol params and utxo supplier. Uses
      * [[EvaluatorMode.EVALUATE_AND_COMPUTE_COST]] mode and [[SlotConfig.Mainnet]].
      * @param protocolParams
      *   Protocol parameters
      * @param utxoSupplier
      *   Utxo supplier
      */
    def this(protocolParams: ProtocolParams, utxoSupplier: UtxoSupplier) =
        this(
          SlotConfig.Mainnet,
          protocolParams,
          utxoSupplier,
          NoScriptSupplier(),
          EvaluatorMode.EVALUATE_AND_COMPUTE_COST
        )

    /** Constructor with protocol params, utxo supplier and script supplier. Uses
      * [[SlotConfig.Mainnet]] and [[EvaluatorMode.EVALUATE_AND_COMPUTE_COST]] mode.
      * @param protocolParams
      *   Protocol parameters
      * @param utxoSupplier
      *   Utxo supplier
      * @param scriptSupplier
      *   Additional script supplier
      */
    def this(
        protocolParams: ProtocolParams,
        utxoSupplier: UtxoSupplier,
        scriptSupplier: ScriptSupplier
    ) =
        this(
          SlotConfig.Mainnet,
          protocolParams,
          utxoSupplier,
          scriptSupplier,
          EvaluatorMode.EVALUATE_AND_COMPUTE_COST
        )

    @BeanProperty
    lazy val costMdls: CostMdls = {
        val costModelV1 = CostModelUtil
            .getCostModelFromProtocolParams(protocolParams, Language.PLUTUS_V1)
            .get()
        val costModelV2 = CostModelUtil
            .getCostModelFromProtocolParams(protocolParams, Language.PLUTUS_V2)
            .get()
        val costModelV3 = CostModelUtil
            .getCostModelFromProtocolParams(protocolParams, Language.PLUTUS_V3)
            .get()
        val cm = CostMdls()
        cm.add(costModelV1)
        cm.add(costModelV2)
        cm.add(costModelV3)
        cm
    }

    // Initialize tx evaluator
    private lazy val txEvaluator =
        val txBudget = ExBudget.fromCpuAndMemory(
          cpu = protocolParams.getMaxTxExSteps.toLong,
          memory = protocolParams.getMaxTxExMem.toLong
        )

        TxEvaluator(
          slotConfig,
          txBudget,
          protocolParams.getProtocolMajorVer.intValue(),
          costMdls,
          mode,
          debugDumpFilesForTesting = false
        )

    override def evaluateTx(
        transaction: Transaction,
        inputUtxos: util.Set[Utxo]
    ): Result[util.List[EvaluationResult]] = {
        val datumHashes = for
            ws <- Option(transaction.getWitnessSet)
            dataList <- Option(ws.getPlutusDataList)
        yield dataList
            .stream()
            .map(data => ByteString.fromArray(data.getDatumHashAsBytes))
            .collect(util.stream.Collectors.toList())
        evaluateTx(transaction, inputUtxos, datumHashes.getOrElse(util.List.of()))
    }

    def evaluateTx(
        transaction: Transaction,
        inputUtxos: util.Set[Utxo],
        datums: util.List[scalus.builtin.ByteString]
    ): Result[util.List[EvaluationResult]] = {
        try {
            val resolvedUtxos: Map[TransactionInput, TransactionOutput] =
                resolveUtxos(transaction, inputUtxos)

            try
                val redeemers =
                    txEvaluator.evaluateTx(transaction, resolvedUtxos, datums.asScala)
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
                case e: TxEvaluationException =>
                    Result
                        .error(s"""Error evaluating transaction: ${e.getMessage}
                               |Evaluation logs: ${e.logs.mkString("\n")}
                               |===========================
                               |""".stripMargin)
                        .asInstanceOf[Result[util.List[EvaluationResult]]]
                case e: Exception =>
                    Result
                        .error(s"Error evaluating transaction: ${e.getMessage}")
                        .asInstanceOf[Result[util.List[EvaluationResult]]]
        } catch {
            case e: Exception => throw ApiException("Error evaluating transaction", e)
        }
    }

    private[bloxbean] def resolveUtxos(
        transaction: Transaction,
        inputUtxos: util.Set[Utxo]
    ): Map[TransactionInput, TransactionOutput] = {
        // initialize utxos with inputUtxos
        val utxos = new mutable.HashMap[TransactionInput, Utxo]()
        for utxo <- inputUtxos.asScala do
            val input = TransactionInput.builder
                .transactionId(utxo.getTxHash)
                .index(utxo.getOutputIndex)
                .build
            utxos.put(input, utxo)

        // Get all input utxos using utxoSupplier
        for input <- transaction.getBody.getInputs.asScala do
            if !utxos.contains(input) then
                val utxo = utxoSupplier.getTxOutput(input.getTransactionId, input.getIndex).get
                utxos.put(input, utxo)

        val scripts = new mutable.HashMap[String, Script]()

        // Get all reference inputs utxos including scripts
        for input <- transaction.getBody.getReferenceInputs.asScala do
            val utxo = utxos.getOrElseUpdate(
              input,
              utxoSupplier.getTxOutput(input.getTransactionId, input.getIndex).get
            )
            // Get reference input script
            if utxo.getReferenceScriptHash != null && scriptSupplier != null then
                try
                    val script = scriptSupplier.getScript(utxo.getReferenceScriptHash)
                    scripts.put(utxo.getReferenceScriptHash, script)
                catch
                    case _: Exception =>
                        // this can happen if the Plutus script is not found
                        // usually this is the case for native scripts
                        // we can ignore this
                        ()

        // Initialize witness set to avoid null pointer exceptions
        if transaction.getWitnessSet == null then
            transaction.setWitnessSet(new TransactionWitnessSet)

        val witnessSet = transaction.getWitnessSet
        if witnessSet.getNativeScripts == null then witnessSet.setNativeScripts(new util.ArrayList)

        if witnessSet.getPlutusV1Scripts == null then
            witnessSet.setPlutusV1Scripts(new util.ArrayList)

        if witnessSet.getPlutusV2Scripts == null then
            witnessSet.setPlutusV2Scripts(new util.ArrayList)

        if witnessSet.getPlutusV3Scripts == null then
            witnessSet.setPlutusV3Scripts(new util.ArrayList)

        if witnessSet.getPlutusDataList == null then
            witnessSet.setPlutusDataList(new util.ArrayList)

        // Resolve Utxos
        val resolvedUtxos =
            val witnessScripts = transaction.getWitnessSet.getPlutusV1Scripts.asScala
                ++ transaction.getWitnessSet.getPlutusV2Scripts.asScala
                ++ transaction.getWitnessSet.getPlutusV3Scripts.asScala
                ++ transaction.getWitnessSet.getNativeScripts.asScala

            for s <- witnessScripts do scripts.put(Utils.bytesToHex(s.getScriptHash), s)

            val allInputs =
                transaction.getBody.getInputs.asScala ++
                    transaction.getBody.getReferenceInputs.asScala

            resolveTxInputs(allInputs, utxos, scripts)
        resolvedUtxos
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
            val utxo = utxos.getOrElse(
              input,
              throw new IllegalStateException(s"Utxo not found for input $input")
            )
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
