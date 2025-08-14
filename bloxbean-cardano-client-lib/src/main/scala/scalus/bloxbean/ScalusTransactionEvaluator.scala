package scalus.bloxbean

import com.bloxbean.cardano.client.api.{TransactionEvaluator, UtxoSupplier}
import com.bloxbean.cardano.client.api.exception.ApiException
import com.bloxbean.cardano.client.api.model.{EvaluationResult, ProtocolParams, Result, Utxo}
import com.bloxbean.cardano.client.api.util.CostModelUtil
import com.bloxbean.cardano.client.plutus.spec.*
import com.bloxbean.cardano.client.transaction.spec.{Transaction, TransactionInput, TransactionOutput}
import com.bloxbean.cardano.client.transaction.util.TransactionUtil
import com.bloxbean.cardano.client.util.JsonUtil
import scalus.builtin.ByteString
import scalus.uplc.eval.ExBudget

import java.util
import java.util.List
import scala.beans.BeanProperty
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional

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
    @BeanProperty val mode: EvaluatorMode,
    @BeanProperty val debugDumpFilesForTesting: Boolean = false
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

    /** Constructor with slot config, protocol params and utxo supplier. Uses
      * [[EvaluatorMode.EVALUATE_AND_COMPUTE_COST]] mode.
      * @param slotConfig
      *   Slot configuration
      * @param protocolParams
      *   Protocol parameters
      * @param utxoSupplier
      *   Utxo supplier
      */
    def this(slotConfig: SlotConfig, protocolParams: ProtocolParams, utxoSupplier: UtxoSupplier) =
        this(
          slotConfig,
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

    /** Constructor with slot config, protocol params, utxo supplier and script supplier. Uses
      * [[EvaluatorMode.EVALUATE_AND_COMPUTE_COST]] mode.
      * @param slotConfig
      *   Slot configuration
      * @param protocolParams
      *   Protocol parameters
      * @param utxoSupplier
      *   Utxo supplier
      * @param scriptSupplier
      *   Additional script supplier
      */
    def this(
        slotConfig: SlotConfig,
        protocolParams: ProtocolParams,
        utxoSupplier: UtxoSupplier,
        scriptSupplier: ScriptSupplier
    ) =
        this(
          slotConfig,
          protocolParams,
          utxoSupplier,
          scriptSupplier,
          EvaluatorMode.EVALUATE_AND_COMPUTE_COST
        )

    @BeanProperty
    lazy val costMdls: CostMdls = {
        val cm = CostMdls()
        CostModelUtil
            .getCostModelFromProtocolParams(protocolParams, Language.PLUTUS_V1)
            .toScala
            .foreach(cm.add)
        CostModelUtil
            .getCostModelFromProtocolParams(protocolParams, Language.PLUTUS_V2)
            .toScala
            .foreach(cm.add)
        CostModelUtil
            .getCostModelFromProtocolParams(protocolParams, Language.PLUTUS_V3)
            .toScala
            .foreach(cm.add)
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
          debugDumpFilesForTesting
        )

    private val utxoResolver = CclUtxoResolver(utxoSupplier, scriptSupplier)

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
        evaluateTx(
          transaction,
          inputUtxos,
          datumHashes.getOrElse(util.List.of()),
          TransactionUtil.getTxHash(transaction)
        )
    }

    def evaluateTx(
        transaction: Transaction,
        inputUtxos: util.Set[Utxo],
        datums: util.List[scalus.builtin.ByteString],
        txhash: String
    ): Result[util.List[EvaluationResult]] = {
        try {
            val resolvedUtxos: Map[TransactionInput, TransactionOutput] =
                utxoResolver.resolveUtxos(transaction, inputUtxos)

            try
                val redeemers =
                    txEvaluator.evaluateTx(transaction, resolvedUtxos, datums.asScala, txhash)
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

    override def evaluateTx(
        cbor: Array[Byte],
        inputUtxos: util.Set[Utxo]
    ): Result[util.List[EvaluationResult]] = {
        evaluateTx(Transaction.deserialize(cbor), inputUtxos)
    }
}
