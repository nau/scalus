package scalus.cardano.ledger

import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.*
import scalus.cardano.ledger.*
import scalus.cardano.ledger.Language.*
import scalus.cardano.ledger.LedgerToPlutusTranslation.*
import scalus.cardano.ledger.utils.{AllNeededScriptHashes, AllResolvedScripts}
import scalus.ledger
import scalus.ledger.api
import scalus.ledger.api.{v1, v2, v3, ScriptContext}
import scalus.uplc.Term.Const
import scalus.uplc.eval.*
import scalus.uplc.{Constant, DeBruijnedProgram, Term}
import scribe.Logger

import java.nio.file.{Files, Paths}
import scala.collection.immutable
import scala.util.control.NonFatal

enum EvaluatorMode extends Enum[EvaluatorMode] {
    case EvaluateAndComputeCost, Validate
}

class PlutusScriptEvaluationException(
    message: String,
    cause: Throwable,
    val logs: Array[String]
) extends RuntimeException(message, cause)

/** Lookup table for resolving scripts and datums during evaluation.
  *
  * @param scripts
  *   Map from script hash to script
  * @param datums
  *   Map from datum hash to datum data
  */
case class LookupTable(
    scripts: Map[ScriptHash, Script],
    // cache of `getDatum`
    datums: Map[DataHash, Data]
)

/** Evaluates Plutus V1, V2 or V3 scripts using the provided transaction and UTxO set.
  *
  * @note
  *   It's an experimental API and may change in future versions, even in patch releases.
  *
  * @param slotConfig
  * @param initialBudget
  * @param protocolMajorVersion
  * @param costModels
  * @param mode
  * @param debugDumpFilesForTesting
  */
class PlutusScriptEvaluator(
    val slotConfig: SlotConfig,
    val initialBudget: ExBudget,
    val protocolMajorVersion: MajorProtocolVersion,
    val costModels: CostModels,
    val mode: EvaluatorMode = EvaluatorMode.EvaluateAndComputeCost,
    val debugDumpFilesForTesting: Boolean = false
) {

    private val log = Logger()
//        .withHandler(minimumLevel = Some(Level.Debug))

    // Lazy-initialized Plutus VMs for different versions
    // Each VM is configured with version-specific cost models and protocol parameters
    private lazy val plutusV1VM =
        PlutusVM.makePlutusV1VM(
          translateMachineParamsFromCostModels(
            costModels,
            PlutusV1,
            protocolMajorVersion
          )
        )

    private lazy val plutusV2VM =
        PlutusVM.makePlutusV2VM(
          translateMachineParamsFromCostModels(
            costModels,
            PlutusV2,
            protocolMajorVersion
          )
        )

    private lazy val plutusV3VM =
        PlutusVM.makePlutusV3VM(
          translateMachineParamsFromCostModels(
            costModels,
            PlutusV3,
            protocolMajorVersion
          )
        )

    /** Evaluates Plutus scripts in a transaction.
      *
      * This is the main evaluation orchestrator that:
      *   1. Extracts redeemers from the transaction
      *   2. Builds datum and script lookup tables
      *   3. Evaluates each redeemer sequentially
      *   4. Tracks total budget consumption
      *   5. Returns all evaluated redeemers
      *
      * @param tx
      *   The transaction containing Plutus scripts and redeemers
      * @param utxos
      *   The UTxO set used for script resolution
      * @return
      *   Seq of evaluated redeemers with updated execution units
      * @throws IllegalStateException
      *   if the transaction does not contain redeemers or if any script resolution fails
      */
    def evalPlutusScripts(
        tx: Transaction,
        utxos: Map[TransactionInput, TransactionOutput],
    ): Seq[Redeemer] =
        evalPlutusScriptsWithContexts(tx, utxos).map(_._1)

    def evalPlutusScriptsWithContexts(
        tx: Transaction,
        utxos: Map[TransactionInput, TransactionOutput],
    ): Seq[(Redeemer, ScriptContext)] = {
        log.debug(s"Starting Phase 2 evaluation for transaction: ${tx.id}")

        val redeemers = tx.witnessSet.redeemers.map(_.value.toMap).getOrElse(Map.empty)

        // Build datum lookup table with hash mapping
        // According to Babbage spec, we lookup datums only in witness set
        // and do not consider reference input inline datums
        // (getDatum, Figure 3: Functions related to scripts)
        val datumsMapping = tx.witnessSet.plutusData.value.toIndexedSeq.view.map { datum =>
            datum.dataHash -> datum.value
        }.toSeq

        val lookupTable = LookupTable(allResolvedScripts(tx, utxos), datumsMapping.toMap)

        log.debug(
          s"Built lookup table with ${lookupTable.scripts.size} scripts and ${lookupTable.datums.size} datums"
        )

        // Evaluate each redeemer
        var remainingBudget = initialBudget

        val neededScriptsData =
            (
              AllNeededScriptHashes.allNeededInputsScriptIndexHashesAndOutputs(tx, utxos) match
                  case Right(inputsScriptIndexHashesAndOutputs) =>
                      inputsScriptIndexHashesAndOutputs.view.map {
                          case (index, scriptHash, output) =>
                              val datum = extractDatumFromOutput(output, lookupTable)
                              (RedeemerTag.Spend, index, scriptHash, datum)
                      }
                  case Left(error) => throw error
            ) ++
                AllNeededScriptHashes.allNeededMintScriptIndexHashesView(tx).map {
                    case (index, scriptHash) =>
                        (RedeemerTag.Mint, index, scriptHash, None)
                } ++
                AllNeededScriptHashes.allNeededVotingProceduresScriptIndexHashesView(tx).map {
                    case (index, scriptHash) =>
                        (RedeemerTag.Voting, index, scriptHash, None)
                } ++
                AllNeededScriptHashes.allNeededWithdrawalsScriptIndexHashesView(tx).map {
                    case (index, scriptHash) =>
                        (RedeemerTag.Reward, index, scriptHash, None)
                } ++
                AllNeededScriptHashes.allNeededProposalProceduresScriptIndexHashesView(tx).map {
                    case (index, scriptHash) =>
                        (RedeemerTag.Proposing, index, scriptHash, None)
                } ++
                AllNeededScriptHashes.allNeededCertificatesScriptIndexHashesView(tx).map {
                    case (index, scriptHash) =>
                        (RedeemerTag.Cert, index, scriptHash, None)
                }

        val evaluatedRedeemers =
            (for
                (redeemerTag, index, scriptHash, datum) <- neededScriptsData
                plutusScript <- lookupTable.scripts.get(scriptHash) match {
                    case Some(plutusScript: PlutusScript) => Some(plutusScript)
                    case Some(other)                      => None
                    case None =>
                        throw new IllegalStateException(s"Script not found: $scriptHash")
                }
            yield {
                if redeemerTag == RedeemerTag.Spend then {
                    // V1 and V2 scripts require datums
                    plutusScript match
                        case _: Script.PlutusV1 | _: Script.PlutusV2 =>
                            if datum.isEmpty then
                                throw new IllegalStateException(
                                  s"Missing required datum for plutus script: $plutusScript"
                                )
                        case _ => // V3 and Native scripts don't require datums in the traditional sense
                }

                val redeemer = redeemers.get((redeemerTag, index)) match
                    case Some(data, exUnits) =>
                        Redeemer(
                          tag = redeemerTag,
                          index = index,
                          data = data,
                          exUnits = exUnits
                        )
                    case None =>
                        throw new IllegalStateException(
                          s"Redeemer not found for tag $redeemerTag and index $index"
                        )

                val (evaluatedRedeemer, sc) =
                    evalRedeemer(tx, datumsMapping, utxos, redeemer, plutusScript, datum)

                // Log execution unit differences for debugging
                if evaluatedRedeemer.exUnits != redeemer.exUnits then
                    log.debug(
                      s"ExUnits changed: ${redeemer.exUnits} -> ${evaluatedRedeemer.exUnits}"
                    )

                // Update remaining budget (safe subtraction as evaluation would fail if budget exceeded)
                remainingBudget = ExBudget.fromCpuAndMemory(
                  remainingBudget.cpu - evaluatedRedeemer.exUnits.steps,
                  remainingBudget.memory - evaluatedRedeemer.exUnits.memory
                )

                (evaluatedRedeemer, sc)
            }).toSeq

        log.debug(s"Phase 2 evaluation completed. Remaining budget: $remainingBudget")
        evaluatedRedeemers
    }

    /** Evaluate a single redeemer and its associated script.
      *
      * This is the core evaluation method that:
      *   1. Resolves the script and datum for the redeemer
      *   2. Builds the appropriate script context for the Plutus version
      *   3. Applies the script arguments (datum, redeemer, context)
      *   4. Executes the script using the appropriate Plutus VM
      *   5. Returns the redeemer with computed execution units
      */
    private def evalRedeemer(
        tx: Transaction,
        datums: Seq[(DataHash, Data)],
        utxos: Map[TransactionInput, TransactionOutput],
        redeemer: Redeemer,
        plutusScript: PlutusScript,
        datum: Option[Data]
    ): (Redeemer, ScriptContext) = {
        val result = plutusScript match
            case Script.PlutusV1(script) =>
                evalPlutusV1Script(tx, datums, utxos, redeemer, script, datum)

            case Script.PlutusV2(script) =>
                evalPlutusV2Script(tx, datums, utxos, redeemer, script, datum)

            case Script.PlutusV3(script) =>
                evalPlutusV3Script(tx, datums, utxos, redeemer, script, datum)

        val cost = result._1.budget
        log.debug(s"Evaluation result: $result")

        // Return redeemer with computed execution units
        redeemer.copy(exUnits = ExUnits(memory = cost.memory, steps = cost.cpu)) -> result._2
    }

    private def extractDatumFromOutput(
        output: TransactionOutput,
        lookupTable: LookupTable
    ): Option[Data] = {
        output match
            case TransactionOutput.Shelley(_, _, Some(datumHash)) =>
                lookupTable.datums.get(datumHash)
            case TransactionOutput.Babbage(_, _, datumOption, _) =>
                datumOption match
                    case Some(DatumOption.Hash(hash))   => lookupTable.datums.get(hash)
                    case Some(DatumOption.Inline(data)) => Some(data)
                    case None                           => None
            case _ => None
    }

    /** Evaluate a Plutus V1 script with the V1 script context.
      */
    private def evalPlutusV1Script(
        tx: Transaction,
        datums: Seq[(ByteString, Data)],
        utxos: Map[TransactionInput, TransactionOutput],
        redeemer: Redeemer,
        script: ByteString,
        datum: Option[Data]
    ): (Result, v1.ScriptContext) = {
        // Build V1 script context
        val txInfoV1 = getTxInfoV1(tx, datums, utxos, slotConfig, protocolMajorVersion)
        val purpose = getScriptPurposeV1(tx, redeemer)
        val scriptContext = v1.ScriptContext(txInfoV1, purpose)
        val ctxData = scriptContext.toData
        val txhash = tx.id.toHex

        log.debug(s"Evaluating PlutusV1 script, purpose: $purpose")
        log.debug(s"Datum: ${datum.map(_.toJson)}")
        log.debug(s"Redeemer: ${redeemer.data.toJson}")
        log.debug(s"Script context: ${ctxData.toJson}")

        // Apply script arguments based on whether datum is present
        evalScript(
          redeemer,
          txhash,
          plutusV1VM,
          script,
          datum.toSeq :+ redeemer.data :+ ctxData*
        ) -> scriptContext
    }

    /** Evaluate a Plutus V2 script with the V2 script context.
      */
    private def evalPlutusV2Script(
        tx: Transaction,
        datums: Seq[(ByteString, Data)],
        utxos: Map[TransactionInput, TransactionOutput],
        redeemer: Redeemer,
        script: ByteString,
        datum: Option[Data]
    ): (Result, v2.ScriptContext) = {
        // Build V2 script context
        val txInfoV2 = getTxInfoV2(tx, datums, utxos, slotConfig, protocolMajorVersion)
        val purpose = getScriptPurposeV2(tx, redeemer)
        val scriptContext = v2.ScriptContext(txInfoV2, purpose)
        val ctxData = scriptContext.toData
        val txhash = tx.id.toHex

        log.debug(s"Evaluating PlutusV2 script, purpose: $purpose")
        log.debug(s"Datum: ${datum.map(_.toJson)}")
        log.debug(s"Redeemer: ${redeemer.data.toJson}")
        log.debug(s"Script context: ${ctxData.toJson}")

        // Apply script arguments
        evalScript(
          redeemer,
          txhash,
          plutusV2VM,
          script,
          datum.toSeq :+ redeemer.data :+ ctxData*
        ) -> scriptContext
    }

    /** Evaluate a Plutus V3 script with the V3 script context.
      */
    private def evalPlutusV3Script(
        tx: Transaction,
        datums: Seq[(ByteString, Data)],
        utxos: Map[TransactionInput, TransactionOutput],
        redeemer: Redeemer,
        script: ByteString,
        datum: Option[Data]
    ): (Result, v3.ScriptContext) = {
        // Build V3 script context
        val txInfoV3 = getTxInfoV3(tx, datums, utxos, slotConfig, protocolMajorVersion)
        val scriptInfo = getScriptInfoV3(tx, redeemer, datum)
        val scriptContext = v3.ScriptContext(txInfoV3, redeemer.data, scriptInfo)
        val ctxData = scriptContext.toData
        val txhash = tx.id.toHex

        log.debug(s"Evaluating PlutusV3 script, scriptInfo: $scriptInfo")
        log.debug(s"Datum: ${datum.map(_.toJson)}")
        log.debug(s"Redeemer: ${redeemer.data.toJson}")
        log.debug(s"Script context: ${ctxData.toJson}")

        // V3 scripts only take the script context as argument
        evalScript(redeemer, txhash, plutusV3VM, script, ctxData) -> scriptContext
    }

    /** Execute a UPLC script with the given arguments.
      *
      * This method handles the low-level script execution:
      *   1. Parses the CBOR-encoded script into a UPLC program
      *   2. Applies the script arguments as constants
      *   3. Executes the program using the appropriate Plutus VM
      *   4. Tracks execution budget and handles budget exhaustion
      *   5. Optionally dumps script data for debugging
      */
    private def evalScript(
        redeemer: Redeemer,
        txhash: String,
        vm: PlutusVM,
        script: ByteString,
        args: Data*
    ): Result = {
        // Parse UPLC program from CBOR
        val program = DeBruijnedProgram.fromCbor(script.bytes)

        // Apply arguments to the program
        val applied = args.foldLeft(program): (acc, arg) =>
            acc $ Const(Constant.Data(arg))

        // Optional debug dumping
        if debugDumpFilesForTesting then
            dumpScriptForDebugging(applied, redeemer, txhash, vm.language)

        // Create budget spender based on evaluation mode
        val spender = mode match
            case EvaluatorMode.EvaluateAndComputeCost => CountingBudgetSpender()
            case EvaluatorMode.Validate               =>
                // Create budget from redeemer execution units
                val budget = ExBudget.fromCpuAndMemory(
                  cpu = redeemer.exUnits.steps,
                  memory = redeemer.exUnits.memory
                )
                RestrictingBudgetSpenderWithScriptDump(budget, debugDumpFilesForTesting)

        val logger = Log()
        // Execute the script
        try
            val resultTerm = vm.evaluateScript(applied, spender, logger)
            Result.Success(resultTerm, spender.getSpentBudget, Map.empty, logger.getLogs.toSeq)
        catch
            case e: StackTraceMachineError =>
                println()
                println(s"Script ${vm.language} ${redeemer.tag} evaluation failed: ${e.getMessage}")
//                println(e.env.view.reverse.take(20).mkString("\n"))
                throw new PlutusScriptEvaluationException(e.getMessage, e, logger.getLogs)
            case NonFatal(e) =>
                throw new PlutusScriptEvaluationException(e.getMessage, e, logger.getLogs)
    }

    /** Dump script information for debugging purposes.
      */
    private def dumpScriptForDebugging(
        program: DeBruijnedProgram,
        redeemer: Redeemer,
        txhash: String,
        language: Language
    ): Unit = {
        Files.write(
          Paths.get(s"script-$txhash-$language-${redeemer.tag}-${redeemer.index}.flat"),
          program.flatEncoded,
          java.nio.file.StandardOpenOption.CREATE,
          java.nio.file.StandardOpenOption.TRUNCATE_EXISTING
        )
    }

    /** Extract all scripts from transaction and UTxOs.
      */
    private def allResolvedScripts(
        tx: Transaction,
        utxos: Map[TransactionInput, TransactionOutput]
    ): Map[ScriptHash, Script] =
        AllResolvedScripts.allResolvedScriptsMap(tx, utxos) match
            case Right(allResolvedScriptsMap) => allResolvedScriptsMap
            case Left(error)                  => throw error

}
