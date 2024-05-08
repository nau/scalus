package scalus.bloxbean

import com.bloxbean.cardano.client.address.{Address, AddressType, Credential, CredentialType}
import com.bloxbean.cardano.client.api.model.Utxo
import com.bloxbean.cardano.client.crypto.Blake2bUtil.blake2bHash224
import com.bloxbean.cardano.client.exception.{CborDeserializationException, CborSerializationException}
import com.bloxbean.cardano.client.plutus.spec.*
import com.bloxbean.cardano.client.transaction.spec.*
import com.bloxbean.cardano.client.transaction.spec.cert.*
import com.bloxbean.cardano.client.transaction.util.TransactionUtil
import io.bullet.borer.{Cbor, Decoder}
import scalus.bloxbean.Interop.toScalusData
import scalus.builtin.{ByteString, Data, JVMPlatformSpecific, PlutusDataCborDecoder, given}
import scalus.ledger
import scalus.ledger.api
import scalus.ledger.api.PlutusLedgerLanguage.*
import scalus.ledger.api.v1.Extended.NegInf
import scalus.ledger.api.v1.{DCert, ScriptPurpose, StakingCredential}
import scalus.ledger.api.{v1, v2, PlutusLedgerLanguage}
import scalus.ledger.babbage.{PlutusV1Params, PlutusV2Params, ProtocolParams}
import scalus.prelude
import scalus.prelude.AssocMap
import scalus.uplc.{Constant, ProgramFlatCodec, Term}
import scalus.uplc.eval.*
import scalus.utils.Hex

import java.math.BigInteger
import java.util
import java.util.*
import java.util.stream.Collectors
import java.util.stream.Stream
import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable, Map}
import scala.jdk.CollectionConverters.*
import scala.math.BigInt

case class SlotConfig(zero_time: Long, zero_slot: Long, slot_length: Long)
object SlotConfig {
    def default: SlotConfig =
        SlotConfig(zero_time = 1660003200000L, zero_slot = 0, slot_length = 1000)
}

class TxEvaluationException(message: String, cause: Throwable) extends Exception(message, cause)

case class ResolvedInput(input: TransactionInput, output: TransactionOutput)

/** Evaluate script costs for a transaction using two phase eval.
  * @note
  *   This is experimental API and subject to change
  */
class TxEvaluator(private val slotConfig: SlotConfig, private val initialBudgetConfig: ExBudget) {

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
        catch case e: Exception => throw new TxEvaluationException("TxEvaluation failed", e)
    }

    private def resolveTxInputs(
        transactionInputs: util.List[TransactionInput],
        utxos: util.Set[Utxo],
        plutusScripts: util.List[PlutusScript]
    ): util.List[TransactionOutput] = {
        transactionInputs.stream
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
        val scripts =
            def decodeToFlat(script: PlutusScript) =
                val decoded = Cbor.decode(Hex.hexToBytes(script.getCborHex)).to[Array[Byte]].value
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
            (v1 ++ v2).toMap

        // FIXME: implement

        /*
         * // discovery in utxos (script ref)

    for utxo in utxos.iter() {
        match &utxo.output {
            TransactionOutput::Legacy(_) => {}
            TransactionOutput::PostAlonzo(output) => {
                if let Some(script) = &output.script_ref {
                    match &script.0 {
                        PseudoScript::NativeScript(ns) => {
                            scripts.insert(ns.compute_hash(), ScriptVersion::Native(ns.clone()));
                        }
                        PseudoScript::PlutusV1Script(v1) => {
                            scripts.insert(v1.compute_hash(), ScriptVersion::V1(v1.clone()));
                        }
                        PseudoScript::PlutusV2Script(v2) => {
                            scripts.insert(v2.compute_hash(), ScriptVersion::V2(v2.clone()));
                        }
                    }
                }
            }
        }
    }
         */
        LookupTable(scripts, datum)
    }

    private def evalPhaseOne(
        tx: Transaction,
        utxos: util.Set[ResolvedInput],
        lookupTable: LookupTable
    ): Unit = {
        val scripts = scriptsNeeded(tx, utxos.asScala)
        println(s"Scrips needed: $scripts")
        validateMissingScripts(scripts, lookupTable.scripts)
        verifyExactSetOfRedeemers(tx, scripts, lookupTable)
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
        if missing.nonEmpty then
            throw new IllegalStateException(s"Missing scripts: $missing")
    }

    private def verifyExactSetOfRedeemers(
        tx: Transaction,
        scripts: AlonzoScriptsNeeded,
        lookupTable: LookupTable
    ): Unit = {
        // Method body goes here
        ??? // not implemented //FIXME: Implement
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

        println(s"Eval redeemer: $purpose")

        val executionPurpose = getExecutionPurpose(utxos, purpose, lookupTable)

        import scalus.bloxbean.Interop.toScalusData
        import scalus.builtin.Data.toData
        import scalus.ledger.api.v1.ToDataInstances.given
        import scalus.ledger.api.v2.ToDataInstances.given
        val result = executionPurpose match
            case ExecutionPurpose.WithDatum(PlutusV1, script, datum) =>
                val machineParams = translateMachineParamsFromCostMdls(costMdls, PlutusV1)
                val rdmr = toScalusData(redeemer.getData)
                val txInfo = getTxInfoV1(tx, utxos, slotConfig)
                val scriptContext = v1.ScriptContext(txInfo, purpose)
                val ctxData = scriptContext.toData
                println(s"Script context: $scriptContext")
                evalScript(machineParams, script, datum, rdmr, ctxData)
            case ExecutionPurpose.WithDatum(PlutusV2, script, datum) =>
                val machineParams = translateMachineParamsFromCostMdls(costMdls, PlutusV2)
                val rdmr = toScalusData(redeemer.getData)
                val txInfo = getTxInfoV2(tx, utxos, slotConfig)
                val scriptContext = v2.ScriptContext(txInfo, purpose)
                val ctxData = scriptContext.toData
                println(s"Script context: $scriptContext")
                evalScript(machineParams, script, datum, rdmr, ctxData)
            case ExecutionPurpose.NoDatum(PlutusV1, script) =>
                val machineParams = translateMachineParamsFromCostMdls(costMdls, PlutusV1)
                val rdmr = toScalusData(redeemer.getData)
                val txInfo = getTxInfoV1(tx, utxos, slotConfig)
                val scriptContext = v1.ScriptContext(txInfo, purpose)
                val ctxData = scriptContext.toData
                println(s"Script context: $scriptContext")
                evalScript(machineParams, script, rdmr, ctxData)
            case ExecutionPurpose.NoDatum(PlutusV2, script) =>
                val machineParams = translateMachineParamsFromCostMdls(costMdls, PlutusV2)
                val rdmr = toScalusData(redeemer.getData)
                val txInfo = getTxInfoV2(tx, utxos, slotConfig)
                val scriptContext = v2.ScriptContext(txInfo, purpose)
                val ctxData = scriptContext.toData
                println(s"Script context: $scriptContext")
                evalScript(machineParams, script, rdmr, ctxData)
            case _ =>
                throw new IllegalStateException(s"Unsupported execution purpose $executionPurpose")

        val cost = result.budget
        println(s"Eval result: $result")
        Redeemer(
          redeemer.getTag,
          redeemer.getIndex,
          redeemer.getData,
          ExUnits(
            BigInteger.valueOf(cost.cpu),
            BigInteger.valueOf(cost.memory)
          )
        )
    }

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
                println(s"logs: ${logger.getLogs.mkString("\n")}")
                println(e.getCekStack.mkString("\n"))
                throw new IllegalStateException("MachineError", e)
    }

    /** Creates [[MachineParams]] from a [[CostMdls]] and a [[PlutusLedgerLanguage]] */
    def translateMachineParamsFromCostMdls(
        costMdls: CostMdls,
        plutus: PlutusLedgerLanguage
    ): MachineParams = {
        import upickle.default.*
        val paramsMap = plutus match
            case PlutusLedgerLanguage.PlutusV1 =>
                val costs = costMdls.get(Language.PLUTUS_V1)
                val params = PlutusV1Params.fromSeq(costs.getCosts.map(_.toInt).toSeq)
                writeJs(params).obj.map { case (k, v) => (k, v.num.toInt) }.toMap
            case PlutusLedgerLanguage.PlutusV2 =>
                val costs = costMdls.get(Language.PLUTUS_V2)
                val params = PlutusV2Params.fromSeq(costs.getCosts.map(_.toInt).toSeq)
                writeJs(params).obj.map { case (k, v) => (k, v.num.toInt) }.toMap
            case PlutusLedgerLanguage.PlutusV3 =>
                throw new NotImplementedError("PlutusV3 not supported yet")

        val builtinCostModel = BuiltinCostModel.fromCostModelParams(paramsMap)
        val machineCosts = CekMachineCosts.fromMap(paramsMap)
        MachineParams(machineCosts = machineCosts, builtinCostModel = builtinCostModel)
    }

    private def getCredential(cred: Credential): v1.Credential = {
        cred.getType match
            case CredentialType.Key =>
                v1.Credential.PubKeyCredential(v1.PubKeyHash(ByteString.fromArray(cred.getBytes)))
            case CredentialType.Script =>
                v1.Credential.ScriptCredential(ByteString.fromArray(cred.getBytes))
    }

    private def getStakingCredential(cred: StakeCredential): v1.StakingCredential = {
        cred.getType match
            case StakeCredType.ADDR_KEYHASH =>
                v1.StakingCredential.StakingHash(
                  v1.Credential.PubKeyCredential(v1.PubKeyHash(ByteString.fromArray(cred.getHash)))
                )
            case StakeCredType.SCRIPTHASH =>
                v1.StakingCredential.StakingHash(
                  v1.Credential.ScriptCredential(ByteString.fromArray(cred.getHash))
                )
    }

    private def getStakingCredential(cred: Credential): v1.StakingCredential = {
        v1.StakingCredential.StakingHash(
          getCredential(cred)
        )
    }

    def getAddress(address: Address): v1.Address = {
        println(s"Get address: ${address.getPaymentCredential.get.getType}")
        val cred = address.getPaymentCredential.map(getCredential).get
        val staking = address.getDelegationCredential
            .map(cred => prelude.Maybe.Just(getStakingCredential(cred)))
            .orElse(prelude.Maybe.Nothing)
        v1.Address(cred, staking)
    }

    private def getTxInInfoV1(
        input: TransactionInput,
        utxos: util.Set[ResolvedInput]
    ): v1.TxInInfo = {
        val out = utxos.asScala
            .find(utxo =>
                utxo.input.getTransactionId == input.getTransactionId && utxo.input.getIndex == input.getIndex
            )
            .map(_.output)
            .getOrElse(throw new IllegalStateException("Input Not Found"))
        val addr = Address(out.getAddress)
        val maybeDatumHash =
            if out.getDatumHash != null then
                prelude.Maybe.Just(ByteString.fromArray(out.getDatumHash))
            else prelude.Maybe.Nothing
        v1.TxInInfo(
          v1.TxOutRef(
            v1.TxId(ByteString.fromHex(input.getTransactionId)),
            input.getIndex
          ),
          v1.TxOut(
            getAddress(addr),
            getValue(out.getValue),
            maybeDatumHash
          )
        )
    }

    private def getTxInInfoV2(
        input: TransactionInput,
        utxos: util.Set[ResolvedInput]
    ): v2.TxInInfo = {
        val out = utxos.asScala
            .find(utxo =>
                utxo.input.getTransactionId == input.getTransactionId && utxo.input.getIndex == input.getIndex
            )
            .map(_.output)
            .getOrElse(throw new IllegalStateException("Input Not Found"))
        val addr = Address(out.getAddress)
        v2.TxInInfo(
          v1.TxOutRef(
            v1.TxId(ByteString.fromHex(input.getTransactionId)),
            input.getIndex
          ),
          v2.TxOut(
            getAddress(addr),
            getValue(out.getValue),
            getOutputDatum(out),
            if out.getScriptRef != null
            then prelude.Maybe.Just(ByteString.fromArray(blake2bHash224(out.getScriptRef)))
            else prelude.Maybe.Nothing
          )
        )
    }

    private def getValue(value: Value): v1.Value = {
        val ma = getValue(value.getMultiAssets)
        if value.getCoin != null then
            val lovelace = v1.Value.lovelace(value.getCoin)
            prelude.AssocMap(
              prelude.List.Cons(
                (ByteString.empty, AssocMap.singleton(ByteString.empty, BigInt(value.getCoin))),
                ma.inner
              )
            )
        else ma
    }

    private def getValue(value: util.List[MultiAsset]): v1.Value = {
        // get sorted multi assets
        val multi = mutable.TreeMap.empty[ByteString, mutable.TreeMap[ByteString, BigInt]]
        for m <- value.asScala do
            val assets = mutable.TreeMap.empty[ByteString, BigInt]
            for asset <- m.getAssets.asScala do
                assets.put(ByteString.fromArray(asset.getNameAsBytes), asset.getValue)
            multi.put(ByteString.fromHex(m.getPolicyId), assets)
        // convert to AssocMap
        val am =
            for (policyId, assets) <- multi.iterator
            yield policyId -> AssocMap(prelude.List.from(assets))

        prelude.AssocMap(prelude.List.from(am))
    }

    private def getTxOutV1(out: TransactionOutput): v1.TxOut = {
        val addr = Address(out.getAddress)
        val maybeDatumHash =
            if out.getDatumHash != null then
                prelude.Maybe.Just(ByteString.fromArray(out.getDatumHash))
            else prelude.Maybe.Nothing
        v1.TxOut(
          getAddress(addr),
          getValue(out.getValue),
          maybeDatumHash
        )
    }

    private def getTxOutV2(out: TransactionOutput): v2.TxOut = {
        val addr = Address(out.getAddress)
        v2.TxOut(
          getAddress(addr),
          getValue(out.getValue),
          getOutputDatum(out),
          if out.getScriptRef != null then
              prelude.Maybe.Just(ByteString.fromArray(out.getScriptRef))
          else prelude.Maybe.Nothing
        )
    }

    private def getOutputDatum(out: TransactionOutput): v2.OutputDatum = {
        if out.getDatumHash != null then
            v2.OutputDatum.OutputDatumHash(ByteString.fromArray(out.getDatumHash))
        else if out.getInlineDatum != null then
            v2.OutputDatum.OutputDatum(toScalusData(out.getInlineDatum))
        else v2.OutputDatum.NoOutputDatum
    }

    extension [A](inline a: A) private inline infix def ??(b: A): A = if a != null then a else b

    private def slotToBeginPosixTime(slot: Long, sc: SlotConfig): Long = {
        val msAfterBegin = (slot - sc.zero_slot) * sc.slot_length
        sc.zero_time + msAfterBegin
    }

    private def getInterval(tx: Transaction, slotConfig: SlotConfig): v1.Interval[v1.POSIXTime] = {
        val validFrom = tx.getBody.getValidityStartInterval
        val lower =
            if validFrom == 0 then v1.LowerBound(NegInf, false)
            else v1.Interval.lowerBound(BigInt(slotToBeginPosixTime(validFrom, slotConfig)))
        val validTo = tx.getBody.getTtl
        val upper =
            if validTo == 0 then v1.UpperBound(NegInf, false)
            else v1.Interval.upperBound(BigInt(slotToBeginPosixTime(validTo, slotConfig)))
        v1.Interval(lower, upper)
    }

    private def getWithdrawals(
        withdrawals: util.List[Withdrawal]
    ): prelude.List[(v1.StakingCredential, BigInt)] = {
        // get sorted withdrawals
        given Ordering[v1.StakingCredential.StakingHash] = Ordering.by { cred =>
            cred.cred match
                case v1.Credential.PubKeyCredential(pkh)  => pkh.hash
                case v1.Credential.ScriptCredential(hash) => hash
        }
        val wdwls = mutable.TreeMap.empty[v1.StakingCredential.StakingHash, BigInt]
        for w <- withdrawals.asScala do
            val cred = Address(w.getRewardAddress).getPaymentCredential.map(getCredential).get
            wdwls.put(v1.StakingCredential.StakingHash(cred), BigInt(w.getCoin))
        prelude.List.from(wdwls)
    }

    private def getDCert(cert: Certificate): v1.DCert = {
        cert match
            case c: StakeRegistration =>
                v1.DCert.DelegRegKey(getStakingCredential(c.getStakeCredential))
            case c: StakeDeregistration =>
                v1.DCert.DelegDeRegKey(getStakingCredential(c.getStakeCredential))
            case c: StakeDelegation =>
                v1.DCert.DelegDelegate(
                  getStakingCredential(c.getStakeCredential),
                  v1.PubKeyHash(ByteString.fromArray(c.getStakePoolId.getPoolKeyHash))
                )
            case c: PoolRegistration =>
                v1.DCert.PoolRegister(
                  v1.PubKeyHash(ByteString.fromArray(c.getOperator)),
                  v1.PubKeyHash(ByteString.fromArray(c.getVrfKeyHash))
                )
            case c: PoolRetirement =>
                v1.DCert.PoolRetire(
                  v1.PubKeyHash(ByteString.fromArray(c.getPoolKeyHash)),
                  BigInt(c.getEpoch)
                )
            case c: GenesisKeyDelegation => v1.DCert.Genesis
            case c: MoveInstataneous     => v1.DCert.Mir
    }

    private def getTxInfoV1(
        tx: Transaction,
        utxos: util.Set[ResolvedInput],
        slotConfig: SlotConfig
    ): v1.TxInfo = {
        val body = tx.getBody
        val certs = body.getCerts ?? util.List.of()
        val rdmrs = tx.getWitnessSet.getRedeemers ?? util.List.of()
        val datums = tx.getWitnessSet.getPlutusDataList ?? util.List.of()
        v1.TxInfo(
          inputs = prelude.List.from(body.getInputs.asScala.map(getTxInInfoV1(_, utxos))),
          outputs = prelude.List.from(body.getOutputs.asScala.map(getTxOutV1)),
          fee = v1.Value.lovelace(body.getFee ?? BigInteger.ZERO),
          mint = getValue(body.getMint ?? util.List.of()),
          dcert = prelude.List.from(certs.asScala.map(getDCert)),
          withdrawals = getWithdrawals(body.getWithdrawals ?? util.List.of()),
          validRange = getInterval(tx, slotConfig),
          signatories = prelude.List.from(
            body.getRequiredSigners.asScala.map { signer =>
                v1.PubKeyHash(ByteString.fromArray(signer))
            }
          ),
          data = prelude.List.from(datums.asScala.map { data =>
              val hash = ByteString.fromArray(data.getDatumHashAsBytes)
              hash -> toScalusData(data)
          }),
          id = v1.TxId(ByteString.fromHex(TransactionUtil.getTxHash(tx)))
        )
    }

    private def getTxInfoV2(
        tx: Transaction,
        utxos: util.Set[ResolvedInput],
        slotConfig: SlotConfig
    ): v2.TxInfo = {
        val body = tx.getBody
        val certs = body.getCerts ?? util.List.of()
        val rdmrs = tx.getWitnessSet.getRedeemers ?? util.List.of()
        val datums = tx.getWitnessSet.getPlutusDataList ?? util.List.of()
        v2.TxInfo(
          inputs = prelude.List.from(body.getInputs.asScala.map(getTxInInfoV2(_, utxos))),
          referenceInputs = prelude.List.from(body.getReferenceInputs.asScala.map { input =>
              getTxInInfoV2(input, utxos)
          }),
          outputs = prelude.List.from(body.getOutputs.asScala.map(getTxOutV2)),
          fee = v1.Value.lovelace(body.getFee ?? BigInteger.ZERO),
          mint = getValue(body.getMint ?? util.List.of()),
          dcert = prelude.List.from(certs.asScala.map(getDCert)),
          withdrawals = AssocMap(getWithdrawals(body.getWithdrawals ?? util.List.of())),
          validRange = getInterval(tx, slotConfig),
          signatories = prelude.List.from(
            body.getRequiredSigners.asScala.map { signer =>
                v1.PubKeyHash(ByteString.fromArray(signer))
            }
          ),
          redeemers = AssocMap(prelude.List.from(rdmrs.asScala.map { redeemer =>
              val purpose = getScriptPurpose(
                redeemer,
                body.getInputs,
                body.getMint,
                body.getCerts,
                body.getWithdrawals
              )
              purpose -> toScalusData(redeemer.getData)
          })),
          data = AssocMap(prelude.List.from(datums.asScala.map { data =>
              val hash = ByteString.fromArray(data.getDatumHashAsBytes)
              hash -> toScalusData(data)
          })),
          id = v1.TxId(ByteString.fromHex(TransactionUtil.getTxHash(tx)))
        )
    }

    private enum ExecutionPurpose:

        case WithDatum(
            scriptVersion: PlutusLedgerLanguage,
            script: VM.ScriptForEvaluation,
            datum: Data
        )

        case NoDatum(scriptVersion: PlutusLedgerLanguage, script: VM.ScriptForEvaluation)

    given Ordering[TransactionInput] with
        def compare(x: TransactionInput, y: TransactionInput): Int =
            x.getTransactionId.compareTo(y.getTransactionId) match
                case 0 => x.getIndex.compareTo(y.getIndex)
                case c => c

    private def getTxOutRefV1(input: TransactionInput) = {
        v1.TxOutRef(
          v1.TxId(ByteString.fromHex(input.getTransactionId)),
          input.getIndex
        )
    }

    private def getScriptPurpose(
        redeemer: Redeemer,
        inputs: util.List[TransactionInput],
        mint: util.List[MultiAsset],
        certificates: util.List[Certificate],
        withdrawals: util.List[Withdrawal]
    ): v1.ScriptPurpose =
        // Cardano Ledger code is stupidly complex and unreadable. We need to make sure this is correct
        val index = redeemer.getIndex.intValue
        redeemer.getTag match
            case RedeemerTag.Spend =>
                val ins = inputs.asScala.sorted
                if ins.isDefinedAt(index) then
                    val input = ins(index)
                    v1.ScriptPurpose.Spending(getTxOutRefV1(input))
                else throw new IllegalStateException(s"Input not found: $index in $inputs")
            case RedeemerTag.Mint =>
                val policyIds = mint.asScala.map(_.getPolicyId).sorted
                if policyIds.isDefinedAt(index) then
                    v1.ScriptPurpose.Minting(ByteString.fromHex(policyIds(index)))
                else throw new IllegalStateException(s"Wrong mint index: $index in $mint")
            case RedeemerTag.Cert =>
                val certs = certificates.asScala
                if certs.isDefinedAt(index) then v1.ScriptPurpose.Certifying(getDCert(certs(index)))
                else throw new IllegalStateException(s"Wrong cert index: $index in $certificates")
            case RedeemerTag.Reward =>
                val rewardAccounts = withdrawals.asScala
                    .map(ra => Address(ra.getRewardAddress))
                    .sortBy(a => ByteString.fromArray(a.getBytes)) // for ordering
                if rewardAccounts.isDefinedAt(index) then
                    val address = rewardAccounts(index)
                    if address.getAddressType == AddressType.Reward then
                        val cred = getCredential(address.getDelegationCredential.get)
                        v1.ScriptPurpose.Rewarding(v1.StakingCredential.StakingHash(cred))
                    else
                        throw new IllegalStateException(
                          s"Wrong reward address type: $address in $withdrawals"
                        )
                else throw new IllegalStateException(s"Wrong reward index: $index in $withdrawals")

    private def getExecutionPurpose(
        utxos: util.Set[ResolvedInput],
        purpose: v1.ScriptPurpose,
        lookupTable: LookupTable
    ): ExecutionPurpose = {
        println(s"Get execution purpose: $purpose, $lookupTable, $utxos")
        purpose match
            case v1.ScriptPurpose.Minting(policyId) =>
                val (lang, script) = lookupTable.scripts.getOrElse(
                  policyId,
                  throw new IllegalStateException("Script Not Found")
                )
                ExecutionPurpose.NoDatum(lang, script)
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
                println(s"Address: ${address.getPaymentCredential.get.getType}")
                val hash = ByteString.fromArray(address.getPaymentCredentialHash.orElseThrow())
                val (version, script) = lookupTable.scripts.getOrElse(
                  hash,
                  throw new IllegalStateException("Script Not Found")
                )
                val datumHash = ByteString.fromArray(utxo.output.getDatumHash)
                val datum = lookupTable.datums.getOrElse(
                  datumHash,
                  throw new IllegalStateException("Datum Not Found")
                )
                ExecutionPurpose.WithDatum(version, script, toScalusData(datum))
            case v1.ScriptPurpose.Rewarding(
                  StakingCredential.StakingHash(v1.Credential.ScriptCredential(hash))
                ) =>
                val (version, script) = lookupTable.scripts.getOrElse(
                  hash,
                  throw new IllegalStateException("Script Not Found")
                )
                ExecutionPurpose.NoDatum(version, script)
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
                        ExecutionPurpose.NoDatum(version, script)
                    case DCert.DelegDelegate(v1.StakingCredential.StakingHash(cred), _) =>
                        val hash = cred match
                            case v1.Credential.ScriptCredential(hash) => hash
                            case _ =>
                                throw new IllegalStateException("OnlyStakeDeregAndDelegAllowed")
                        val (version, script) = lookupTable.scripts.getOrElse(
                          hash,
                          throw new IllegalStateException("Script Not Found")
                        )
                        ExecutionPurpose.NoDatum(version, script)
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
        println(s"Eval phase two $tx, $utxos, $costMdls, $initialBudget, $slotConfig, $runPhaseOne")
        val redeemers = tx.getWitnessSet.getRedeemers ?? util.List.of()
        val lookupTable = getScriptAndDatumLookupTable(tx, utxos)
        println(s"Lookup table: $lookupTable")

        if runPhaseOne then
            // Subset of phase 1 check on redeemers and scripts
            evalPhaseOne(tx, utxos, lookupTable)

        var remainingBudget = initialBudget
        val collectedRedeemers = redeemers.asScala.map { redeemer =>
            val evaluatedRedeemer = evalRedeemer(
              tx,
              utxos,
              slotConfig,
              redeemer,
              lookupTable,
              costMdls,
              remainingBudget
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
