package scalus.cardano.ledger

import org.slf4j.LoggerFactory
import scalus.bloxbean.{EvaluatorMode, RestrictingBudgetSpenderWithScripDump, TxEvaluationException}
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.*
import scalus.cardano.ledger.*
import scalus.cardano.ledger.Language.*
import scalus.cardano.ledger.LedgerToPlutusTranslation.*
import scalus.cardano.ledger.utils.AllWitnessesScripts
import scalus.ledger
import scalus.ledger.api
import scalus.ledger.api.{v1, v2, v3, MajorProtocolVersion}
import scalus.uplc.Term.Const
import scalus.uplc.eval.*
import scalus.uplc.{Constant, DeBruijnedProgram, Term}

import java.nio.file.{Files, Paths}
import scala.collection.immutable

private[scalus] class PlutusScriptEvaluator(
    val slotConfig: SlotConfig,
    val initialBudget: ExBudget,
    val protocolMajorVersion: MajorProtocolVersion,
    val costModels: CostModels,
    val mode: EvaluatorMode = EvaluatorMode.EVALUATE_AND_COMPUTE_COST,
    val debugDumpFilesForTesting: Boolean = false
) {

    /** Lookup table for resolving scripts and datums during evaluation.
      *
      * @param scripts
      *   Map from script hash to script
      * @param datums
      *   Map from datum hash to datum data
      */
    case class LookupTable(
        scripts: Map[ScriptHash, Script],
        datums: Map[DataHash, Data]
    )

    private val log = LoggerFactory.getLogger(getClass.getName)

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

    /** Resolve the script and datum associated with a redeemer.
      *
      * This method implements the core script resolution logic, mapping redeemer tags and indices
      * to their corresponding scripts and associated data.
      *
      * The resolution logic varies by redeemer tag:
      *   - Spend: Resolves to script in UTxO being spent, includes datum if script requires it
      *   - Mint: Resolves to minting policy script
      *   - Cert: Resolves to certificate-related script
      *   - Reward: Resolves to staking script for withdrawal
      *   - Proposing: Resolves to governance proposal script
      *   - Voting: Resolves to voter credential script
      *
      * @param tx
      *   The transaction containing the redeemer
      * @param redeemer
      *   The redeemer to resolve
      * @param lookupTable
      *   Script and datum lookup table
      * @param utxos
      *   UTxO set for script resolution
      * @return
      *   Tuple of (Script, Optional[Datum])
      */
    private def findScript(
        tx: Transaction,
        redeemer: Redeemer,
        lookupTable: LookupTable,
        utxos: Map[TransactionInput, TransactionOutput]
    ): (Script, Option[Data]) = {
        val index = redeemer.index
        redeemer.tag match
            case RedeemerTag.Spend =>
                findSpendScript(tx, index, lookupTable, utxos)
            case RedeemerTag.Mint =>
                // FIXME:
                findSpendScript(tx, index, lookupTable, utxos)
            case RedeemerTag.Cert =>
                // FIXME:
                findSpendScript(tx, index, lookupTable, utxos)
            case RedeemerTag.Reward =>
                // FIXME:
                val withdrawals = tx.body.value.withdrawals.get.withdrawals.toArray.sortBy(_._1)
                if !withdrawals.isDefinedAt(index) then
                    throw new IllegalStateException(
                      s"Withdrawal not found: $index in ${withdrawals.mkString("[", ", ", "]")}"
                    )
                val scriptHash = withdrawals(index)._1.address.scriptHash.get
                lookupTable.scripts(scriptHash) -> None

            case RedeemerTag.Voting =>
                // FIXME:
                findSpendScript(tx, index, lookupTable, utxos)
            case RedeemerTag.Proposing =>
                // FIXME:
                findSpendScript(tx, index, lookupTable, utxos)

    }

    /** Find script for spending a UTxO (Spend redeemer tag).
      *
      * For spending scripts, we need to:
      *   1. Locate the UTxO being spent using the redeemer index
      *   2. Extract the script hash from the UTxO's address
      *   3. Resolve the script from the lookup table
      *   4. Extract the datum if the script requires it (V1/V2 scripts need datums)
      */
    private def findSpendScript(
        tx: Transaction,
        index: Int,
        lookupTable: LookupTable,
        utxos: Map[TransactionInput, TransactionOutput]
    ): (Script, Option[Data]) = {
        val inputs = tx.body.value.inputs.toArray.sorted // FIXME sorted

        if !inputs.isDefinedAt(index) then
            throw new IllegalStateException(
              s"Input not found: $index in ${inputs.mkString("[", ", ", "]")}"
            )

        val input = inputs(index)
        val output = utxos.getOrElse(
          input,
          throw new IllegalStateException(s"UTxO not found for input: $input")
        )

        // Extract script hash from address
        val scriptHash = output.address.scriptHash
            .getOrElse(
              throw new IllegalStateException(s"No script credential in address: ${output.address}")
            )

        // Resolve script
        val script = lookupTable.scripts.getOrElse(
          scriptHash,
          throw new IllegalStateException(s"Script not found: $scriptHash")
        )

        // Extract datum if needed
        val datum = extractDatumFromOutput(output, lookupTable)

        // V1 and V2 scripts require datums
        script match
            case _: Script.PlutusV1 | _: Script.PlutusV2 =>
                if datum.isEmpty then
                    throw new IllegalStateException(s"Missing required datum for script: $script")
            case _ => // V3 and Native scripts don't require datums in the traditional sense
        (script, datum)
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
        datums: collection.Seq[(ByteString, Data)],
        utxos: Map[TransactionInput, TransactionOutput],
        redeemer: Redeemer,
        lookupTable: LookupTable
    ): Redeemer = {
        val result = findScript(tx, redeemer, lookupTable, utxos) match
            case (_: Script.Native, _) =>
                throw new IllegalStateException("Native script evaluation not supported in Phase 2")

            case (Script.PlutusV1(script), datum) =>
                evalPlutusV1Script(tx, datums, utxos, redeemer, script, datum)

            case (Script.PlutusV2(script), datum) =>
                evalPlutusV2Script(tx, datums, utxos, redeemer, script, datum)

            case (Script.PlutusV3(script), datum) =>
                evalPlutusV3Script(tx, datums, utxos, redeemer, script, datum)

        val cost = result.budget
        log.debug(s"Evaluation result: $result")

        // Return redeemer with computed execution units
        redeemer.copy(exUnits = ExUnits(memory = cost.memory, steps = cost.cpu))
    }

    /** Evaluate a Plutus V1 script with the V1 script context.
      */
    private def evalPlutusV1Script(
        tx: Transaction,
        datums: collection.Seq[(ByteString, Data)],
        utxos: Map[TransactionInput, TransactionOutput],
        redeemer: Redeemer,
        script: ByteString,
        datum: Option[Data]
    ): Result = {
        // Build V1 script context
        val txInfoV1 = getTxInfoV1(tx, datums, utxos, slotConfig, protocolMajorVersion)
        val purpose = getScriptPurposeV1(tx, redeemer)
        val scriptContext = v1.ScriptContext(txInfoV1, purpose)
        val ctxData = scriptContext.toData

        if log.isDebugEnabled() then
            log.debug(s"Evaluating PlutusV1 script, purpose: $purpose")
            log.debug(s"Datum: ${datum.map(_.toJson)}")
            log.debug(s"Redeemer: ${redeemer.data.toJson}")
            log.debug(s"Script context: ${ctxData.toJson}")

        // Apply script arguments based on whether datum is present
        evalScript(redeemer, plutusV1VM, script, datum.toSeq :+ redeemer.data :+ ctxData*)
    }

    /** Evaluate a Plutus V2 script with the V2 script context.
      */
    private def evalPlutusV2Script(
        tx: Transaction,
        datums: collection.Seq[(ByteString, Data)],
        utxos: Map[TransactionInput, TransactionOutput],
        redeemer: Redeemer,
        script: ByteString,
        datum: Option[Data]
    ): Result = {
        // Build V2 script context
        val txInfoV2 = getTxInfoV2(tx, datums, utxos, slotConfig, protocolMajorVersion)
        val purpose = getScriptPurposeV2(tx, redeemer)
        val scriptContext = v2.ScriptContext(txInfoV2, purpose)
        val ctxData = scriptContext.toData

        if log.isDebugEnabled() then
            log.debug(s"Evaluating PlutusV2 script, purpose: $purpose")
            log.debug(s"Datum: ${datum.map(_.toJson)}")
            log.debug(s"Redeemer: ${redeemer.data.toJson}")
            log.debug(s"Script context: ${ctxData.toJson}")

        // Apply script arguments
        evalScript(redeemer, plutusV2VM, script, datum.toSeq :+ redeemer.data :+ ctxData*)
    }

    /** Evaluate a Plutus V3 script with the V3 script context.
      */
    private def evalPlutusV3Script(
        tx: Transaction,
        datums: collection.Seq[(ByteString, Data)],
        utxos: Map[TransactionInput, TransactionOutput],
        redeemer: Redeemer,
        script: ByteString,
        datum: Option[Data]
    ): Result = {
        // Build V3 script context
        val txInfoV3 = getTxInfoV3(tx, datums, utxos, slotConfig, protocolMajorVersion)
        val scriptInfo = getScriptInfoV3(tx, redeemer, datum)
        val scriptContext = v3.ScriptContext(txInfoV3, redeemer.data, scriptInfo)
        val ctxData = scriptContext.toData

        if log.isDebugEnabled() then
            log.debug(s"Evaluating PlutusV3 script, scriptInfo: $scriptInfo")
            log.debug(s"Datum: ${datum.map(_.toJson)}")
            log.debug(s"Redeemer: ${redeemer.data.toJson}")
            log.debug(s"Script context: ${ctxData.toJson}")

        // V3 scripts only take the script context as argument
        evalScript(redeemer, plutusV3VM, script, ctxData)
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
        if debugDumpFilesForTesting then dumpScriptForDebugging(applied, redeemer)

        // Create budget spender based on evaluation mode
        val spender = mode match
            case EvaluatorMode.EVALUATE_AND_COMPUTE_COST => CountingBudgetSpender()
            case EvaluatorMode.VALIDATE                  =>
                // Create budget from redeemer execution units
                val budget = ExBudget.fromCpuAndMemory(
                  cpu = redeemer.exUnits.steps,
                  memory = redeemer.exUnits.memory
                )
                RestrictingBudgetSpenderWithScripDump(budget, debugDumpFilesForTesting)

        val logger = Log()
        // Execute the script
        try
            val resultTerm = vm.evaluateScript(applied, spender, logger)
            Result.Success(resultTerm, spender.getSpentBudget, Map.empty, logger.getLogs.toSeq)
        catch
            case e: Exception =>
                throw new TxEvaluationException(e.getMessage, e, logger.getLogs)
    }

    /** Dump script information for debugging purposes.
      */
    private def dumpScriptForDebugging(program: DeBruijnedProgram, redeemer: Redeemer): Unit = {
        Files.write(
          Paths.get(s"script-${redeemer.tag}-${redeemer.index}.flat"),
          program.flatEncoded,
          java.nio.file.StandardOpenOption.CREATE,
          java.nio.file.StandardOpenOption.TRUNCATE_EXISTING
        )
    }

    /** Perform Phase 2 transaction evaluation.
      *
      * This is the main evaluation orchestrator that:
      *   1. Extracts redeemers from the transaction
      *   2. Builds datum and script lookup tables
      *   3. Evaluates each redeemer sequentially
      *   4. Tracks total budget consumption
      *   5. Returns all evaluated redeemers
      */
    def evalPhaseTwo(
        tx: Transaction,
        utxos: Map[TransactionInput, TransactionOutput],
    ): collection.Seq[Redeemer] = {
        log.debug(s"Starting Phase 2 evaluation for transaction: ${tx.id}")

        val redeemers = tx.witnessSet.redeemers
            .getOrElse(throw new IllegalStateException("Transaction does not contain redeemers"))
            .value
            .toIndexedSeq // FIXME: should be sorted?

        // Build datum lookup table with hash mapping
        val datumsMapping = tx.witnessSet.plutusData.value.view.map { datum =>
            datum.dataHash -> datum.value
        }.toSeq

        val lookupTable =
            val scripts = getAllResolvedScripts(tx, utxos)
            LookupTable(scripts, datumsMapping.toMap)

        log.debug(
          s"Built lookup table with ${lookupTable.scripts.size} scripts and ${lookupTable.datums.size} datums"
        )

        // Evaluate each redeemer
        var remainingBudget = initialBudget
        val evaluatedRedeemers = for redeemer <- redeemers yield
            val evaluatedRedeemer = evalRedeemer(tx, datumsMapping, utxos, redeemer, lookupTable)

            // Log execution unit differences for debugging
            if evaluatedRedeemer.exUnits != redeemer.exUnits then
                log.debug(s"ExUnits changed: ${redeemer.exUnits} -> ${evaluatedRedeemer.exUnits}")

            // Update remaining budget (safe subtraction as evaluation would fail if budget exceeded)
            remainingBudget = ExBudget.fromCpuAndMemory(
              remainingBudget.cpu - evaluatedRedeemer.exUnits.steps,
              remainingBudget.memory - evaluatedRedeemer.exUnits.memory
            )

            evaluatedRedeemer

        log.debug(s"Phase 2 evaluation completed. Remaining budget: $remainingBudget")
        evaluatedRedeemers
    }

    // Placeholder methods for building script contexts and purposes
    // These would need to be implemented based on the actual scalus.ledger.api structures

    // Helper methods

    /** Extract all scripts from transaction and UTxOs.
      */
    private def getAllResolvedScripts(
        tx: Transaction,
        utxos: Map[TransactionInput, TransactionOutput]
    ): Map[ScriptHash, Script] =
        val provided = AllWitnessesScripts
            .allWitnessesScriptsView(tx)
            .map { script => script.scriptHash -> script }
            .toMap
        val referenceScripts = utxos.values.flatMap { output =>
            output.scriptRef.map { case ScriptRef(script) =>
                script.scriptHash -> script
            }
        }.toMap
        provided ++ referenceScripts
}
