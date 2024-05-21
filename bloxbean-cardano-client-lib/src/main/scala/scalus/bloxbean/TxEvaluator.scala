package scalus.bloxbean

import com.bloxbean.cardano.client.address.{Address, AddressType, CredentialType}
import com.bloxbean.cardano.client.common.cbor.CborSerializationUtil
import com.bloxbean.cardano.client.plutus.spec.*
import com.bloxbean.cardano.client.transaction.spec.*
import com.bloxbean.cardano.client.transaction.util.TransactionUtil
import io.bullet.borer.{Cbor, Decoder}
import org.slf4j.LoggerFactory
import scalus.bloxbean.Interop.*
import scalus.builtin.{ByteString, Data, JVMPlatformSpecific, given}
import scalus.ledger
import scalus.ledger.api
import scalus.ledger.api.PlutusLedgerLanguage.*
import scalus.ledger.api.v1.{DCert, ScriptPurpose, StakingCredential}
import scalus.ledger.api.{v1, v2, PlutusLedgerLanguage}
import scalus.uplc.Program
import scalus.uplc.Term.Apply
import scalus.uplc.Term.Const
import scalus.uplc.{Constant, ProgramFlatCodec, Term}
import scalus.uplc.eval.*
import scalus.utils.Hex
import scalus.utils.Utils
import upickle.default.*

import java.math.BigInteger
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util
import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}
import scala.jdk.CollectionConverters.*

case class SlotConfig(zero_time: Long, zero_slot: Long, slot_length: Long)
object SlotConfig {
    def default: SlotConfig =
        SlotConfig(zero_time = 1596059091000L, zero_slot = 4492800, slot_length = 1000)
}

class TxEvaluationException(message: String, cause: Throwable) extends Exception(message, cause)

given ReadWriter[Data] = readwriter[ujson.Value].bimap(
  {
      case Data.Constr(constr, args) =>
          ujson.Obj(
            "constructor" -> ujson.Num(constr),
            "fields" -> ujson.Arr(ArrayBuffer.from(args.map(writeJs)))
          )
      case Data.Map(values) =>
          ujson.Obj("map" -> ujson.Arr(ArrayBuffer.from(values.map { case (k, v) =>
              ujson.Obj("k" -> writeJs(k), "v" -> writeJs(v))
          })))
      case Data.List(values) =>
          ujson.Obj("list" -> ujson.Arr(ArrayBuffer.from(values.map(writeJs))))
      case Data.I(value) =>
          val v = if value.isValidLong then writeJs(value.toLong) else writeJs(value)
          ujson.Obj("int" -> v)
      case Data.B(value) => ujson.Obj("bytes" -> writeJs(value.toHex))
  },
  json =>
      if json.obj.get("constructor").isDefined then
          Data.Constr(
            json.obj("constructor").num.toLong,
            json.obj("fields").arr.map(f => read[Data](f)).toList
          )
      else if json.obj.get("map").isDefined then
          Data.Map(
            json.obj("map")
                .arr
                .map { obj =>
                    val k = read[Data](obj.obj("k"))
                    val v = read[Data](obj.obj("v"))
                    k -> v
                }
                .toList
          )
      else if json.obj.get("list").isDefined then
          Data.List(json.obj("list").arr.map(e => read[Data](e)).toList)
      else if json.obj.get("int").isDefined then Data.I(json.obj("int").num.toLong)
      else if json.obj.get("bytes").isDefined then Data.B(ByteString.fromHex(json.obj("bytes").str))
      else throw new Exception("Invalid Data")
)

enum ScriptVersion:
    case Native
    case PlutusV1(flatScript: ByteString)
    case PlutusV2(flatScript: ByteString)

/** Evaluate script costs for a transaction using two phase eval.
  * @note
  *   This is experimental API and subject to change
  */
class TxEvaluator(
    val slotConfig: SlotConfig,
    val initialBudget: ExBudget,
    val protocolMajorVersion: Int,
    val costMdls: CostMdls,
    val failOnBudgetMismatch: Boolean = false,
    val debugDumpFilesForTesting: Boolean = false
) {
    private val log = LoggerFactory.getLogger(getClass.getName)
    // TODO: implement proper error handling, exceptions, logging etc

    /** Phase 2 validation and execution of the transaction
      */

    def evaluateTx(
        transaction: Transaction,
        inputUtxos: Map[TransactionInput, TransactionOutput]
    ): collection.Seq[Redeemer] = {
        val datumHashes = transaction.getWitnessSet.getPlutusDataList.asScala
            .map(data => ByteString.fromArray(data.getDatumHashAsBytes))
            .toSeq
        evaluateTx(transaction, inputUtxos, datumHashes)
    }

    def evaluateTx(
        transaction: Transaction,
        inputUtxos: Map[TransactionInput, TransactionOutput],
        originalDatumHashes: collection.Seq[ByteString]
    ): collection.Seq[Redeemer] = {

        // For debugging, store ins and outs in cbor format
        // to run aiken simulator
        if debugDumpFilesForTesting then
            val txhash = TransactionUtil.getTxHash(transaction)
            Files.write(Paths.get(s"tx-$txhash.cbor"), transaction.serialize())
            Files.deleteIfExists(java.nio.file.Paths.get("scalus.log"))
            storeInsOutsInCborFiles(inputUtxos)
            // run modified aiken for testing
            println(
              s"cargo run -- tx simulate --cbor ~/projects/scalus/bloxbean-cardano-client-lib/tx-$txhash.cbor ~/projects/scalus/bloxbean-cardano-client-lib/ins.cbor ~/projects/scalus/bloxbean-cardano-client-lib/outs.cbor > ~/projects/scalus/bloxbean-cardano-client-lib/aiken.log"
            )

        evalPhaseTwo(
          transaction,
          originalDatumHashes,
          inputUtxos,
          runPhaseOne = true
        )
    }

    private def storeInsOutsInCborFiles(
        utxos: Map[TransactionInput, TransactionOutput]
    ): Unit = {
        val ins = co.nstant.in.cbor.model.Array()
        val outs = co.nstant.in.cbor.model.Array()

        for (in, out) <- utxos do
            ins.add(in.serialize())
            outs.add(out.serialize())

        Files.write(Path.of("ins.cbor"), CborSerializationUtil.serialize(ins));
        Files.write(Path.of("outs.cbor"), CborSerializationUtil.serialize(outs));
    }

    private type ScriptHash = ByteString
    private type Hash = ByteString
    private case class LookupTable(
        scripts: collection.Map[ScriptHash, ScriptVersion],
        datums: collection.Map[Hash, PlutusData]
    )

    private def getScriptAndDatumLookupTable(
        tx: Transaction,
        originalDatumHashes: collection.Seq[ByteString],
        utxos: Map[TransactionInput, TransactionOutput]
    ): LookupTable = {
        val datums = originalDatumHashes.zip(tx.getWitnessSet.getPlutusDataList.asScala).toMap

        val scripts = {
            def decodeToFlat(script: PlutusScript) =
                // unwrap the outer CBOR encoding
                val decoded = Cbor.decode(Hex.hexToBytes(script.getCborHex)).to[Array[Byte]].value
                // and decode the inner CBOR encoding. Don't ask me why.
                ByteString.fromArray(Cbor.decode(decoded).to[Array[Byte]].value)

            val native = tx.getWitnessSet.getNativeScripts.asScala
                .map: script =>
                    ByteString.fromArray(script.getScriptHash) -> ScriptVersion.Native

            val v1 = tx.getWitnessSet.getPlutusV1Scripts.asScala
                .map: script =>
                    val flatScript = decodeToFlat(script)
                    ByteString.fromArray(script.getScriptHash) -> ScriptVersion.PlutusV1(flatScript)
            val v2 = tx.getWitnessSet.getPlutusV2Scripts.asScala
                .map: script =>
                    val flatScript = decodeToFlat(script)
                    ByteString.fromArray(script.getScriptHash) -> ScriptVersion.PlutusV2(flatScript)
            native ++ v1 ++ v2
        }

        val referenceScripts = ArrayBuffer.empty[(ScriptHash, ScriptVersion)]
        for output <- utxos.values do
            if output.getScriptRef != null then
                val scriptInfo = Interop.getScriptInfoFromScriptRef(output.getScriptRef)
                referenceScripts += scriptInfo.hash -> scriptInfo.scriptVersion

        val allScripts = (scripts ++ referenceScripts).toMap
        LookupTable(allScripts, datums)
    }

    private def evalPhaseOne(
        tx: Transaction,
        utxos: Map[TransactionInput, TransactionOutput],
        lookupTable: LookupTable
    ): Unit = {
        val scripts = scriptsNeeded(tx, utxos)
        validateMissingScripts(scripts, lookupTable.scripts)
        verifyExactSetOfRedeemers(tx, scripts, lookupTable.scripts)
    }

    private type AlonzoScriptsNeeded = immutable.Seq[(v1.ScriptPurpose, ScriptHash)]

    private def scriptsNeeded(
        tx: Transaction,
        utxos: Map[TransactionInput, TransactionOutput]
    ): AlonzoScriptsNeeded = {
        val needed = ArrayBuffer.empty[(v1.ScriptPurpose, ScriptHash)]
        val txb = tx.getBody

        for input <- txb.getInputs.asScala do
            val output = utxos.getOrElse(input, throw new IllegalStateException("Input not found"))
            val address = Address(output.getAddress)

            // if we spend a script output, we need the script
            if address.getPaymentCredential.get.getType == CredentialType.Script then
                needed += v1.ScriptPurpose.Spending(getTxOutRefV1(input)) -> ByteString.fromArray(
                  address.getPaymentCredentialHash.orElseThrow()
                )

        for withdrawal <- txb.getWithdrawals.asScala do
            val address = Address(withdrawal.getRewardAddress)
            if address.getAddressType == AddressType.Reward then
                getCredential(address.getDelegationCredential.get) match
                    case cred @ api.v1.Credential.ScriptCredential(hash) =>
                        needed += v1.ScriptPurpose.Rewarding(
                          v1.StakingCredential.StakingHash(cred)
                        ) -> hash
                    case _ =>

        for cert <- txb.getCerts.asScala do
            val c = getDCert(cert)
            c match
                case v1.DCert.DelegDeRegKey(
                      v1.StakingCredential.StakingHash(v1.Credential.ScriptCredential(hash))
                    ) =>
                    needed += v1.ScriptPurpose.Certifying(c) -> hash
                case v1.DCert.DelegDelegate(
                      v1.StakingCredential.StakingHash(v1.Credential.ScriptCredential(hash)),
                      _
                    ) =>
                    needed += v1.ScriptPurpose.Certifying(c) -> hash
                case _ =>

        for mint <- txb.getMint.asScala do
            val policyId = ByteString.fromHex(mint.getPolicyId)
            needed += v1.ScriptPurpose.Minting(policyId) -> policyId

        needed.toSeq
    }

    private def validateMissingScripts(
        scripts: AlonzoScriptsNeeded,
        txScripts: collection.Map[ScriptHash, ScriptVersion]
    ): Unit = {
        val received = txScripts.keySet
        val needed = scripts.map(_._2).toSet
        val missing = needed.diff(received)
        if missing.nonEmpty then throw new IllegalStateException(s"Missing scripts: $missing")
    }

    private def verifyExactSetOfRedeemers(
        tx: Transaction,
        scripts: AlonzoScriptsNeeded,
        txScripts: collection.Map[ScriptHash, ScriptVersion]
    ): Unit = {
        // FIXME: implement
    }

    /// builds a redeemer pointer (tag, index) from a script purpose by setting the tag
    /// according to the type of the script purpose, and the index according to the
    /// placement of script purpose inside its container.
    private def buildRedeemerPtr(
        tx: Transaction,
        purpose: v1.ScriptPurpose
    ): Option[Redeemer] = {
        ??? // FIXME: implement
    }

    private def evalRedeemer(
        tx: Transaction,
        utxos: Map[TransactionInput, TransactionOutput],
        redeemer: Redeemer,
        lookupTable: LookupTable,
        remainingBudget: ExBudget
    ): Redeemer = {
        val purpose = getScriptPurpose(
          redeemer,
          tx.getBody.getInputs,
          tx.getBody.getMint,
          tx.getBody.getCerts,
          tx.getBody.getWithdrawals
        )

//        log.debug(s"Eval redeemer: $purpose")

        val executionPurpose = getExecutionPurpose(utxos, purpose, lookupTable)

        import scalus.bloxbean.Interop.toScalusData
        import scalus.builtin.Data.toData
        import scalus.ledger.api.v1.ToDataInstances.given
        import scalus.ledger.api.v2.ToDataInstances.given
        val result = executionPurpose match
            case ExecutionPurpose.WithDatum(ScriptVersion.PlutusV1(script), scriptHash, datum) =>
                log.debug(s"eval: PlutusV1, $scriptHash ${purpose}: ${write(datum)}")
                val machineParams = translateMachineParamsFromCostMdls(costMdls, PlutusV1)
                val rdmr = toScalusData(redeemer.getData)
                log.debug(s"Redeemer: ${write(rdmr)}")
                val txInfo = getTxInfoV1(tx, utxos, slotConfig, protocolMajorVersion)
                val scriptContext = v1.ScriptContext(txInfo, purpose)
                val ctxData = scriptContext.toData
                log.debug(s"Script context: ${write(ctxData)}")
                evalScript(redeemer, machineParams, script.bytes, datum, rdmr, ctxData)
            case ExecutionPurpose.WithDatum(ScriptVersion.PlutusV2(script), scriptHash, datum) =>
//                log.debug(
//                  s"eval: PlutusV2, $scriptHash ${purpose}: ${PrettyPrinter.pretty(datum).render(100)}"
//                )
                val machineParams = translateMachineParamsFromCostMdls(costMdls, PlutusV2)
                val rdmr = toScalusData(redeemer.getData)
                val txInfo = getTxInfoV2(tx, utxos, slotConfig, protocolMajorVersion)
                val scriptContext = v2.ScriptContext(txInfo, purpose)
                val ctxData = scriptContext.toData
                log.debug(s"Script context: ${write(ctxData)}")
                evalScript(redeemer, machineParams, script.bytes, datum, rdmr, ctxData)
            case ExecutionPurpose.NoDatum(ScriptVersion.PlutusV1(script), scriptHash) =>
                log.debug(s"eval: PlutusV1, $scriptHash ${purpose}")
                val machineParams = translateMachineParamsFromCostMdls(costMdls, PlutusV1)
                val rdmr = toScalusData(redeemer.getData)
                val txInfo = getTxInfoV1(tx, utxos, slotConfig, protocolMajorVersion)
                val scriptContext = v1.ScriptContext(txInfo, purpose)
                val ctxData = scriptContext.toData
                log.debug(s"Script context: ${write(ctxData)}")
                evalScript(redeemer, machineParams, script.bytes, rdmr, ctxData)
            case ExecutionPurpose.NoDatum(ScriptVersion.PlutusV2(script), scriptHash) =>
                log.debug(s"eval: PlutusV2, $scriptHash ${purpose}")
                val machineParams = translateMachineParamsFromCostMdls(costMdls, PlutusV2)
                val rdmr = toScalusData(redeemer.getData)
                val txInfo = getTxInfoV2(tx, utxos, slotConfig, protocolMajorVersion)
                val scriptContext = v2.ScriptContext(txInfo, purpose)
                val ctxData = scriptContext.toData
                log.debug(s"Script context: ${write(ctxData)}")
                evalScript(redeemer, machineParams, script.bytes, rdmr, ctxData)
            case _ =>
                throw new IllegalStateException(s"Unsupported execution purpose $executionPurpose")

        val cost = result.budget
        log.debug(s"Eval result: $result")
        Redeemer(
          redeemer.getTag,
          redeemer.getIndex,
          redeemer.getData,
          ExUnits(
            BigInteger.valueOf(cost.memory),
            BigInteger.valueOf(cost.cpu)
          )
        )
    }

    final class RestrictingBudgetSpender2(val maxBudget: ExBudget) extends BudgetSpender {
        private var cpuLeft: Long = maxBudget.cpu
        private var memoryLeft: Long = maxBudget.memory

        def spendBudget(cat: ExBudgetCategory, budget: ExBudget, env: CekValEnv): Unit = {
            if debugDumpFilesForTesting then
                cat match
                    case ExBudgetCategory.BuiltinApp(fun) =>
                        Files.write(
                          java.nio.file.Paths.get("scalus.log"),
                          s"fun $$${fun}, cost: ExBudget { mem: ${budget.memory}, cpu: ${budget.cpu} }\n".getBytes,
                          java.nio.file.StandardOpenOption.CREATE,
                          java.nio.file.StandardOpenOption.APPEND
                        )
                    case _ =>
            cpuLeft -= budget.cpu
            memoryLeft -= budget.memory
            if cpuLeft < 0 || memoryLeft < 0 then throw new OutOfExBudgetError(maxBudget, env)
        }

        def getSpentBudget: ExBudget =
            ExBudget.fromCpuAndMemory(maxBudget.cpu - cpuLeft, maxBudget.memory - memoryLeft)

        def reset(): Unit = {
            cpuLeft = maxBudget.cpu
            memoryLeft = maxBudget.memory
        }
    }

    private def evalScript(
        redeemer: Redeemer,
        machineParams: MachineParams,
        script: VM.ScriptForEvaluation,
        args: Data*
    ) = {
        val budget = ExBudget.fromCpuAndMemory(
          cpu = redeemer.getExUnits.getSteps.longValue,
          memory = redeemer.getExUnits.getMem.longValue
        )
        val program = ProgramFlatCodec.decodeFlat(script)
        val applied = args.foldLeft(program.term) { (acc, arg) =>
            Apply(acc, Const(scalus.uplc.DefaultUni.asConstant(arg)))
        }
        if debugDumpFilesForTesting then
            val flat = ProgramFlatCodec.unsafeEncodeFlat(Program((1, 0, 0), applied))
            Files.write(
              java.nio.file.Paths.get("script.flat"),
              flat,
              java.nio.file.StandardOpenOption.CREATE
            )
        val spender = RestrictingBudgetSpender2(budget)
        val logger = Log()
        val cek = new CekMachine(machineParams, spender, logger, JVMPlatformSpecific)
        val resultTerm = cek.evaluateTerm(applied)
        val r = CekResult(resultTerm, spender.getSpentBudget, logger.getLogs)
        if failOnBudgetMismatch && r.budget != budget then
            log.warn(
              s"Budget mismatch: expected $budget, got ${r.budget}"
            )
            throw new RuntimeException(
              s"Budget mismatch: expected $budget, got ${r.budget}"
            )
        r
    }

    private def getExecutionPurpose(
        utxos: Map[TransactionInput, TransactionOutput],
        purpose: v1.ScriptPurpose,
        lookupTable: LookupTable
    ): ExecutionPurpose = {
//        log.debug(s"Get execution purpose: $purpose, $lookupTable, $utxos")
        purpose match
            case v1.ScriptPurpose.Minting(policyId) =>
                val scriptVersion = lookupTable.scripts.getOrElse(
                  policyId,
                  throw new IllegalStateException("Script Not Found")
                )
                ExecutionPurpose.NoDatum(scriptVersion, policyId)
            case v1.ScriptPurpose.Spending(txOutRef) =>
                val output = utxos
                    .getOrElse(
                      TransactionInput
                          .builder()
                          .transactionId(txOutRef.id.hash.toHex)
                          .index(txOutRef.idx.toInt)
                          .build(),
                      throw new IllegalStateException("Input Not Found: " + txOutRef)
                    )
                val address = Address(output.getAddress)
                val hash = ByteString.fromArray(address.getPaymentCredentialHash.orElseThrow())
                val scriptVersion = lookupTable.scripts.getOrElse(
                  hash,
                  throw new IllegalStateException("Script Not Found")
                )
                (output.getDatumHash, output.getInlineDatum) match
                    case (null, null) =>
                        throw new IllegalStateException("Datum Not Found")
                    case (datumHash, null) =>
                        val datum = lookupTable.datums.getOrElse(
                          ByteString.fromArray(datumHash),
                          throw new IllegalStateException(
                            s"Datum Hash Not Found ${Utils.bytesToHex(datumHash)}"
                          )
                        )
                        ExecutionPurpose.WithDatum(scriptVersion, hash, toScalusData(datum))
                    case (null, inlineDatum) =>
                        ExecutionPurpose.WithDatum(scriptVersion, hash, toScalusData(inlineDatum))
                    case _ =>
                        throw new IllegalStateException(
                          "Output can't have both inline datum and datum hash"
                        )
            case v1.ScriptPurpose.Rewarding(
                  StakingCredential.StakingHash(v1.Credential.ScriptCredential(hash))
                ) =>
                val scriptVersion = lookupTable.scripts.getOrElse(
                  hash,
                  throw new IllegalStateException("Script Not Found")
                )
                ExecutionPurpose.NoDatum(scriptVersion, hash)
            case ScriptPurpose.Rewarding(stakingCred) =>
                throw new IllegalStateException("OnlyStakeDeregAndDelegAllowed")
            case v1.ScriptPurpose.Certifying(cert) =>
                cert match
                    case v1.DCert.DelegDeRegKey(v1.StakingCredential.StakingHash(cred)) =>
                        val hash = cred match
                            case v1.Credential.ScriptCredential(hash) => hash
                            case _ =>
                                throw new IllegalStateException("OnlyStakeDeregAndDelegAllowed")
                        val scriptVersion = lookupTable.scripts.getOrElse(
                          hash,
                          throw new IllegalStateException("Script Not Found")
                        )
                        ExecutionPurpose.NoDatum(scriptVersion, hash)
                    case DCert.DelegDelegate(v1.StakingCredential.StakingHash(cred), _) =>
                        val hash = cred match
                            case v1.Credential.ScriptCredential(hash) => hash
                            case _ =>
                                throw new IllegalStateException("OnlyStakeDeregAndDelegAllowed")
                        val scriptVersion = lookupTable.scripts.getOrElse(
                          hash,
                          throw new IllegalStateException("Script Not Found")
                        )
                        ExecutionPurpose.NoDatum(scriptVersion, hash)
                    case _ => throw new IllegalStateException("OnlyStakeDeregAndDelegAllowed")
    }

    private def evalPhaseTwo(
        tx: Transaction,
        originalDatumHashes: collection.Seq[ByteString],
        utxos: Map[TransactionInput, TransactionOutput],
        runPhaseOne: Boolean
    ): collection.Seq[Redeemer] = {
        log.debug(
          s"Eval phase two $tx, $utxos, $costMdls, $initialBudget, $slotConfig, $runPhaseOne"
        )
        val redeemers = tx.getWitnessSet.getRedeemers ?? util.List.of()
        val lookupTable = getScriptAndDatumLookupTable(tx, originalDatumHashes, utxos)
        log.debug(s"Lookup table: $lookupTable")

        if runPhaseOne then
            // Subset of phase 1 check on redeemers and scripts
            evalPhaseOne(tx, utxos, lookupTable)

        var remainingBudget = initialBudget
        val collectedRedeemers = for redeemer <- redeemers.asScala yield {
            val evaluatedRedeemer = evalRedeemer(
              tx,
              utxos,
              redeemer,
              lookupTable,
              remainingBudget
            )

            if evaluatedRedeemer.getExUnits.getSteps != redeemer.getExUnits.getSteps
                || evaluatedRedeemer.getExUnits.getMem != redeemer.getExUnits.getMem
            then
                log.warn(
                  s"ExUnits: ${redeemer.getExUnits} - Evaluated: ${evaluatedRedeemer.getExUnits}"
                )

            // The subtraction is safe here as ex units counting is done during evaluation.
            // Redeemer would fail already if budget was negative.
            remainingBudget = ExBudget.fromCpuAndMemory(
              remainingBudget.cpu - evaluatedRedeemer.getExUnits.getSteps.longValue,
              remainingBudget.memory - evaluatedRedeemer.getExUnits.getMem.longValue
            )

            evaluatedRedeemer
        }
        collectedRedeemers
    }
}
