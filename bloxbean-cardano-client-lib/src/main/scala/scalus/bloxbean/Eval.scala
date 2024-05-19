package scalus.bloxbean

import com.bloxbean.cardano.client.address.{Address, AddressType, CredentialType}
import com.bloxbean.cardano.client.api.model.Utxo
import com.bloxbean.cardano.client.crypto.Blake2bUtil.blake2bHash224
import com.bloxbean.cardano.client.exception.{CborDeserializationException, CborSerializationException}
import com.bloxbean.cardano.client.plutus.spec.*
import com.bloxbean.cardano.client.transaction.spec.*
import io.bullet.borer.{Cbor, Decoder}
import scalus.bloxbean.Interop.*
import scalus.builtin.{ByteString, Data, JVMPlatformSpecific, given}
import scalus.ledger
import scalus.ledger.api
import scalus.ledger.api.PlutusLedgerLanguage.*
import scalus.ledger.api.v1.{DCert, ScriptPurpose, StakingCredential}
import scalus.ledger.api.{v1, v2, PlutusLedgerLanguage}
import scalus.sir.PrettyPrinter
import scalus.uplc.{Constant, ProgramFlatCodec, Term}
import scalus.uplc.eval.*
import scalus.utils.Hex
import scalus.utils.Utils
import upickle.default.*

import java.math.BigInteger
import java.util
import java.util.*
import java.util.stream.Collectors
import java.util.stream.Stream
import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable, Map}
import scala.jdk.CollectionConverters.*

case class SlotConfig(zero_time: Long, zero_slot: Long, slot_length: Long)
object SlotConfig {
    def default: SlotConfig =
        SlotConfig(zero_time = 1596059091000L, zero_slot = 4492800, slot_length = 1000)
}

class TxEvaluationException(message: String, cause: Throwable) extends Exception(message, cause)

case class ResolvedInput(input: TransactionInput, output: TransactionOutput)

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
      case Data.I(value) => ujson.Obj("int" -> writeJs(value))
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

/** Evaluate script costs for a transaction using two phase eval.
  * @note
  *   This is experimental API and subject to change
  */
class TxEvaluator(private val slotConfig: SlotConfig, private val initialBudgetConfig: ExBudget) {
    // TODO: implement proper error handling, exceptions, logging etc
    val failedScripts = mutable.Set[String]()
    val succScripts = mutable.Set[String]()

    /** Evaluate script costs for a transaction
      *
      * @param transaction
      *   Transaction
      * @param inputUtxos
      *   List utxos used in transaction inputs
      * @param scripts
      *   Plutus Scripts in transaction
      * @param costMdls
      *   Cost models
      * @return
      *   List of [[Redeemer]] with estimated script costs as
      *   [[com.bloxbean.cardano.client.plutus.spec.ExUnits]]
      * @throws TxEvaluationException
      *   if script evaluation fails
      */
    def evaluateTx(
        transaction: Transaction,
        inputUtxos: util.Set[Utxo],
        scripts: util.List[PlutusScript],
        costMdls: CostMdls
    ): util.List[Redeemer] = {
        val witnessScripts = new util.ArrayList[PlutusScript]
        witnessScripts.addAll(transaction.getWitnessSet.getPlutusV1Scripts)
        witnessScripts.addAll(transaction.getWitnessSet.getPlutusV2Scripts)

        val allScripts =
            Stream.concat(scripts.stream, witnessScripts.stream).collect(Collectors.toList)

        val txInputs = transaction.getBody.getInputs
        val refTxInputs = transaction.getBody.getReferenceInputs
        val allInputs =
            Stream.concat(txInputs.stream, refTxInputs.stream).collect(Collectors.toList)
        val txOutputs = resolveTxInputs(allInputs, inputUtxos, allScripts)
        val utxo = allInputs.asScala
            .zip(txOutputs.asScala)
            .map { case (input, output) =>
                ResolvedInput(input, output)
            }
            .toSet
            .asJava

        try
            evalPhaseTwo(
              transaction,
              utxo,
              costMdls,
              initialBudgetConfig,
              slotConfig,
              runPhaseOne = true
            )
        catch
            case e: Exception =>
                println(s"Error evaluating transaction: ${e.getMessage}")
                e.printStackTrace()
                throw new TxEvaluationException("TxEvaluation failed", e)
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
                    try Hex.bytesToHex(script.getScriptHash) == utxo.getReferenceScriptHash
                    catch case e: CborSerializationException => throw new IllegalStateException(e)
                }

                val inlineDatum = Option(utxo.getInlineDatum).map(hex =>
                    PlutusData.deserialize(Hex.hexToBytes(hex))
                )
                val datumHash = Option(utxo.getDataHash).map(Hex.hexToBytes)

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

    private type ScriptHash = ByteString
    private type Hash = ByteString
    private case class LookupTable(
        scripts: collection.Map[ScriptHash, (PlutusLedgerLanguage, Array[Byte])],
        datums: collection.Map[Hash, PlutusData]
    )

    private def getScriptAndDatumLookupTable(
        tx: Transaction,
        utxos: util.Set[ResolvedInput]
    ): LookupTable = {
        val datum = tx.getWitnessSet.getPlutusDataList.asScala
            .map: data =>
                ByteString.fromArray(data.getDatumHashAsBytes) -> data
            .toMap
        val scripts = {
            def decodeToFlat(script: PlutusScript) =
                // unwrap the outer CBOR encoding
                val decoded = Cbor.decode(Hex.hexToBytes(script.getCborHex)).to[Array[Byte]].value
                // and decode the inner CBOR encoding. Don't ask me why.
                Cbor.decode(decoded).to[Array[Byte]].value

            // FIXME: add native scripts
            val v1 = tx.getWitnessSet.getPlutusV1Scripts.asScala
                .map: script =>
                    val flatScript = decodeToFlat(script)
                    ByteString.fromArray(
                      script.getScriptHash
                    ) -> (PlutusLedgerLanguage.PlutusV1, flatScript)
            val v2 = tx.getWitnessSet.getPlutusV2Scripts.asScala
                .map: script =>
                    val flatScript = decodeToFlat(script)
                    ByteString.fromArray(
                      script.getScriptHash
                    ) -> (PlutusLedgerLanguage.PlutusV2, flatScript)
            v1 ++ v2
        }

        val referenceScripts = ArrayBuffer.empty[(ScriptHash, (PlutusLedgerLanguage, Array[Byte]))]

        for utxo <- utxos.asScala do
            if utxo.output.getScriptRef != null then
                // script_ref is incoded as CBOR Array
                val (scriptType, scriptCbor) =
                    Cbor.decode(utxo.output.getScriptRef).to[(Byte, Array[Byte])].value
                // and script hash is calculated from the script type byte and the scriptCbor
                val scriptBytesForScriptHash = Array(scriptType) ++ scriptCbor
                val hash = ByteString.fromArray(blake2bHash224(scriptBytesForScriptHash))
                scriptType match
                    case 0 =>
                        // FIXME: implement Timelock script
                        ???
                    case 1 => // Plutus V1
                        val script = Cbor.decode(scriptCbor).to[Array[Byte]].value
                        referenceScripts += hash -> (PlutusLedgerLanguage.PlutusV1, script)
                    case 2 => // Plutus V2
                        val script = Cbor.decode(scriptCbor).to[Array[Byte]].value
                        referenceScripts += hash -> (PlutusLedgerLanguage.PlutusV2, script)

        val allScripts = (scripts ++ referenceScripts).toMap
        LookupTable(allScripts, datum)
    }

    private def evalPhaseOne(
        tx: Transaction,
        utxos: util.Set[ResolvedInput],
        lookupTable: LookupTable
    ): Unit = {
        val scripts = scriptsNeeded(tx, utxos.asScala)
//        println(s"Scrips needed: $scripts")
        validateMissingScripts(scripts, lookupTable.scripts)
        verifyExactSetOfRedeemers(tx, scripts, lookupTable.scripts)
    }

    private type AlonzoScriptsNeeded = immutable.Seq[(v1.ScriptPurpose, ScriptHash)]

    private def scriptsNeeded(
        tx: Transaction,
        utxos: collection.Set[ResolvedInput]
    ): AlonzoScriptsNeeded = {
        val needed = ArrayBuffer.empty[(v1.ScriptPurpose, ScriptHash)]
        val txb = tx.getBody

        for input <- txb.getInputs.asScala do
            // TODO: refoactor utxos ot Map?
            val utxo = utxos
                .find(_.input == input)
                .getOrElse(
                  throw new IllegalStateException("Input not found")
                )
            val address = Address(utxo.output.getAddress)

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
        txScripts: collection.Map[ScriptHash, (PlutusLedgerLanguage, Array[Byte])]
    ): Unit = {
        val received = txScripts.keySet
        val needed = scripts.map(_._2).toSet
        val missing = needed.diff(received)
        if missing.nonEmpty then throw new IllegalStateException(s"Missing scripts: $missing")
    }

    private def verifyExactSetOfRedeemers(
        tx: Transaction,
        scripts: AlonzoScriptsNeeded,
        txScripts: collection.Map[ScriptHash, (PlutusLedgerLanguage, Array[Byte])]
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
        utxos: util.Set[ResolvedInput],
        slotConfig: SlotConfig,
        redeemer: Redeemer,
        lookupTable: LookupTable,
        costMdls: CostMdls,
        remainingBudget: ExBudget
    ): Redeemer = {
        val purpose = getScriptPurpose(
          redeemer,
          tx.getBody.getInputs,
          tx.getBody.getMint,
          tx.getBody.getCerts,
          tx.getBody.getWithdrawals
        )

//        println(s"Eval redeemer: $purpose")

        val executionPurpose = getExecutionPurpose(utxos, purpose, lookupTable)

        import scalus.bloxbean.Interop.toScalusData
        import scalus.builtin.Data.toData
        import scalus.ledger.api.v1.ToDataInstances.given
        import scalus.ledger.api.v2.ToDataInstances.given
        val result = executionPurpose match
            case ExecutionPurpose.WithDatum(PlutusV1, scriptHash, script, datum) =>
//                println(
//                  s"eval: PlutusV1, $scriptHash ${purpose}: ${PrettyPrinter.pretty(datum).render(100)}"
//                )
                val machineParams = translateMachineParamsFromCostMdls(costMdls, PlutusV1)
                val rdmr = toScalusData(redeemer.getData)
                val txInfo = getTxInfoV1(tx, utxos, slotConfig)
                val scriptContext = v1.ScriptContext(txInfo, purpose)
                val ctxData = scriptContext.toData
//                println(s"Script context: ${write(ctxData)}")
                evalScript(machineParams, script, datum, rdmr, ctxData)
            case ExecutionPurpose.WithDatum(PlutusV2, scriptHash, script, datum) =>
//                println(
//                  s"eval: PlutusV2, $scriptHash ${purpose}: ${PrettyPrinter.pretty(datum).render(100)}"
//                )
                val machineParams = translateMachineParamsFromCostMdls(costMdls, PlutusV2)
                val rdmr = toScalusData(redeemer.getData)
                val txInfo = getTxInfoV2(tx, utxos, slotConfig)
                val scriptContext = v2.ScriptContext(txInfo, purpose)
                val ctxData = scriptContext.toData
//                println(s"Script context: $scriptContext")
                evalScript(machineParams, script, datum, rdmr, ctxData)
            case ExecutionPurpose.NoDatum(PlutusV1, scriptHash, script) =>
//                println(s"eval: PlutusV1, $scriptHash ${purpose}")
                val machineParams = translateMachineParamsFromCostMdls(costMdls, PlutusV1)
                val rdmr = toScalusData(redeemer.getData)
                val txInfo = getTxInfoV1(tx, utxos, slotConfig)
                val scriptContext = v1.ScriptContext(txInfo, purpose)
                val ctxData = scriptContext.toData
//                println(s"Script context: $scriptContext")
                evalScript(machineParams, script, rdmr, ctxData)
            case ExecutionPurpose.NoDatum(PlutusV2, scriptHash, script) =>
//                println(s"eval: PlutusV2, $scriptHash ${purpose}")
                val machineParams = translateMachineParamsFromCostMdls(costMdls, PlutusV2)
                val rdmr = toScalusData(redeemer.getData)
                val txInfo = getTxInfoV2(tx, utxos, slotConfig)
                val scriptContext = v2.ScriptContext(txInfo, purpose)
                val ctxData = scriptContext.toData
//                println(s"Script context: $scriptContext")
                evalScript(machineParams, script, rdmr, ctxData)
            case _ =>
                throw new IllegalStateException(s"Unsupported execution purpose $executionPurpose")

        val cost = result.budget
        println(s"${Console.GREEN}Eval result: $result${Console.RESET}")
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

    /** FIXME: refactor VM.evaluateScriptCounting to return logs on error, then remove this method
      */
    private def evalScript(
        machineParams: MachineParams,
        script: VM.ScriptForEvaluation,
        args: Data*
    ) = {
        val spender = CountingBudgetSpender()
        val logger = Log()
        val cek = new CekMachine(
          machineParams,
          spender,
          logger,
          JVMPlatformSpecific
        )
        try
            val program = ProgramFlatCodec.decodeFlat(script)
            val applied = args.foldLeft(program.term) { (acc, arg) =>
                Term.Apply(acc, Term.Const(Constant.Data(arg)))
            }
            val resultTerm = cek.evaluateTerm(applied)
            CekResult(resultTerm, spender.getSpentBudget, logger.getLogs)
        catch
            case e: StackTraceMachineError =>
                println(s"${Console.RED}Machine Error: ${e.getMessage}${Console.RESET}")
                /*println(
                  e.env
                      .map({
                          case (k, v @ CekValue.VCon(c)) =>
                              s"$k -> ${PrettyPrinter.pretty(c).render(80)}"
                          case (k, v: CekValue.VDelay)              => s"$k -> delay"
                          case (k, v: CekValue.VLamAbs)             => s"$k -> lam"
                          case (k, v @ CekValue.VBuiltin(bi, _, _)) => s"$k -> $bi"
                      })
                      .mkString("\n")
                )*/
                println(s"logs: ${logger.getLogs.mkString("\n")}")
                throw new IllegalStateException("MachineError", e)
    }

    private def getExecutionPurpose(
        utxos: util.Set[ResolvedInput],
        purpose: v1.ScriptPurpose,
        lookupTable: LookupTable
    ): ExecutionPurpose = {
//        println(s"Get execution purpose: $purpose, $lookupTable, $utxos")
        purpose match
            case v1.ScriptPurpose.Minting(policyId) =>
                val (lang, script) = lookupTable.scripts.getOrElse(
                  policyId,
                  throw new IllegalStateException("Script Not Found")
                )
                ExecutionPurpose.NoDatum(lang, policyId, script)
            case v1.ScriptPurpose.Spending(txOutRef) =>
                val utxo = utxos.asScala
                    .find(utxo =>
                        ByteString.fromHex(
                          utxo.input.getTransactionId
                        ) == txOutRef.id.hash && utxo.input.getIndex == txOutRef.idx.toInt
                    )
                    .getOrElse(
                      throw new IllegalStateException("Input Not Found: " + txOutRef)
                    )
                val address = Address(utxo.output.getAddress)
                val hash = ByteString.fromArray(address.getPaymentCredentialHash.orElseThrow())
                val (version, script) = lookupTable.scripts.getOrElse(
                  hash,
                  throw new IllegalStateException("Script Not Found")
                )
                (utxo.output.getDatumHash, utxo.output.getInlineDatum) match
                    case (null, null) =>
                        throw new IllegalStateException("Datum Not Found")
                    case (datumHash, null) =>
                        val datum = lookupTable.datums.getOrElse(
                          ByteString.fromArray(datumHash),
                          throw new IllegalStateException(
                            s"Datum Hash Not Found ${Utils.bytesToHex(datumHash)}"
                          )
                        )
                        ExecutionPurpose.WithDatum(version, hash, script, toScalusData(datum))
                    case (null, inlineDatum) =>
                        ExecutionPurpose.WithDatum(version, hash, script, toScalusData(inlineDatum))
                    case _ =>
                        throw new IllegalStateException(
                          "Output can't have both inline datum and datum hash"
                        )
            case v1.ScriptPurpose.Rewarding(
                  StakingCredential.StakingHash(v1.Credential.ScriptCredential(hash))
                ) =>
                val (version, script) = lookupTable.scripts.getOrElse(
                  hash,
                  throw new IllegalStateException("Script Not Found")
                )
                ExecutionPurpose.NoDatum(version, hash, script)
            case ScriptPurpose.Rewarding(stakingCred) =>
                throw new IllegalStateException("OnlyStakeDeregAndDelegAllowed")
            case v1.ScriptPurpose.Certifying(cert) =>
                cert match
                    case v1.DCert.DelegDeRegKey(v1.StakingCredential.StakingHash(cred)) =>
                        val hash = cred match
                            case v1.Credential.ScriptCredential(hash) => hash
                            case _ =>
                                throw new IllegalStateException("OnlyStakeDeregAndDelegAllowed")
                        val (version, script) = lookupTable.scripts.getOrElse(
                          hash,
                          throw new IllegalStateException("Script Not Found")
                        )
                        ExecutionPurpose.NoDatum(version, hash, script)
                    case DCert.DelegDelegate(v1.StakingCredential.StakingHash(cred), _) =>
                        val hash = cred match
                            case v1.Credential.ScriptCredential(hash) => hash
                            case _ =>
                                throw new IllegalStateException("OnlyStakeDeregAndDelegAllowed")
                        val (version, script) = lookupTable.scripts.getOrElse(
                          hash,
                          throw new IllegalStateException("Script Not Found")
                        )
                        ExecutionPurpose.NoDatum(version, hash, script)
                    case _ => throw new IllegalStateException("OnlyStakeDeregAndDelegAllowed")
    }

    private def evalPhaseTwo(
        tx: Transaction,
        utxos: util.Set[ResolvedInput],
        costMdls: CostMdls,
        initialBudget: ExBudget,
        slotConfig: SlotConfig,
        runPhaseOne: Boolean
    ): util.List[Redeemer] = {
//        println(s"Eval phase two $tx, $utxos, $costMdls, $initialBudget, $slotConfig, $runPhaseOne")
        val redeemers = tx.getWitnessSet.getRedeemers ?? util.List.of()
        val lookupTable = getScriptAndDatumLookupTable(tx, utxos)
//        println(s"Lookup table: $lookupTable")

        if runPhaseOne then
            // Subset of phase 1 check on redeemers and scripts
            evalPhaseOne(tx, utxos, lookupTable)

        var remainingBudget = initialBudget
        val collectedRedeemers = redeemers.asScala.map { redeemer =>
//            println(PrettyPrinter.pretty(Interop.toScalusData(redeemer.getData)).render(100))
            val evaluatedRedeemer = evalRedeemer(
              tx,
              utxos,
              slotConfig,
              redeemer,
              lookupTable,
              costMdls,
              remainingBudget
            )

            if evaluatedRedeemer.getExUnits.getSteps != redeemer.getExUnits.getSteps || evaluatedRedeemer.getExUnits.getMem != redeemer.getExUnits.getMem
            then
                println(
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

        collectedRedeemers.asJava
    }
}
