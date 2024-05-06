package scalus.bloxbean

import com.bloxbean.cardano.client.address.{Address, Credential, CredentialType}
import com.bloxbean.cardano.client.api.model.Utxo
import com.bloxbean.cardano.client.exception.{CborDeserializationException, CborSerializationException}
import com.bloxbean.cardano.client.plutus.spec.*
import com.bloxbean.cardano.client.transaction.spec.*
import com.bloxbean.cardano.client.transaction.spec.cert.*
import com.bloxbean.cardano.client.transaction.util.TransactionUtil
import io.bullet.borer.{Cbor, Decoder}
import scalus.bloxbean.Interop.toScalusData
import scalus.builtin.{ByteString, Data, JVMPlatformSpecific, PlutusDataCborDecoder, given}
import scalus.ledger.api.v1.Extended.NegInf
import scalus.ledger.api.{v1, v2}
import scalus.prelude
import scalus.prelude.AssocMap
import scalus.uplc.ProgramFlatCodec
import scalus.uplc.eval.*
import scalus.utils.Hex

import java.math.BigInteger
import java.util
import java.util.*
import java.util.stream.Collectors
import scala.collection.{Map, mutable}
import scala.jdk.CollectionConverters.*
import scala.math.BigInt

case class SlotConfig(zero_time: Long, zero_slot: Long, slot_length: Long)
object SlotConfig {
    def default: SlotConfig =
        SlotConfig(zero_time = 1660003200000L, zero_slot = 0, slot_length = 1000)
}

class TxEvaluationException(message: String, cause: Throwable) extends Exception(message, cause)

/** Evaluate script costs for a transaction using two phase eval.
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
      *   List of {@link Redeemer} with estimated script costs as {@link
      *   com.bloxbean.cardano.client.plutus.spec.ExUnits}
      * @throws TxEvaluationException
      *   if script evaluation fails
      */
    def evaluateTx(
        transaction: Transaction,
        inputUtxos: Set[Utxo],
        scripts: List[PlutusScript],
        costMdls: CostMdls
    ): List[Redeemer] = {
        val witnessScripts = new ArrayList[PlutusScript]()
        witnessScripts.addAll(transaction.getWitnessSet.getPlutusV1Scripts)
        witnessScripts.addAll(transaction.getWitnessSet.getPlutusV2Scripts)

        val allScripts = if scripts != null && !scripts.isEmpty then
            val all = new ArrayList[PlutusScript]()
            all.addAll(scripts)
            all.addAll(witnessScripts)
            all
        else witnessScripts
        try {
            evalPhaseTwo(transaction, inputUtxos, costMdls, initialBudgetConfig, slotConfig, true)
        } catch {
            case e: Exception => throw new TxEvaluationException("TxEvaluation failed", e)
        }
    }

    private def resolveTxInputs(
        transactionInputs: List[TransactionInput],
        utxos: Set[Utxo],
        plutusScripts: List[PlutusScript]
    ): List[TransactionOutput] = {
        transactionInputs
            .stream()
            .map { input =>
                val utxo = utxos.asScala
                    .find(_utxo =>
                        input.getTransactionId == _utxo.getTxHash && input.getIndex == _utxo.getOutputIndex
                    )
                    .getOrElse(throw new IllegalStateException())

                val address = utxo.getAddress

                val plutusScript = plutusScripts.asScala.find { script =>
                    try {
                        Hex.bytesToHex(script.getScriptHash) == utxo.getReferenceScriptHash
                    } catch {
                        case e: CborSerializationException => throw new IllegalStateException(e)
                    }
                }

                val inlineDatum = Option(utxo.getInlineDatum).map(hex =>
                    PlutusData.deserialize(Hex.hexToBytes(hex))
                )
                val datumHash = Option(utxo.getDataHash).map(Hex.hexToBytes)

                try {
                    TransactionOutput.builder
                        .address(address)
                        .value(utxo.toValue)
                        .datumHash(if inlineDatum.isEmpty then datumHash.orNull else null)
                        .inlineDatum(inlineDatum.orNull)
                        .scriptRef(plutusScript.orNull)
                        .build()
                } catch {
                    case e: CborDeserializationException => throw new IllegalStateException(e)
                }
            }
            .collect(Collectors.toList())
    }

    type Hash = ByteString
    case class LookupTable(
        scripts: scala.collection.Map[Hash, (Language, Array[Byte])],
        datums: scala.collection.Map[Hash, PlutusData]
    )

    def getScriptAndDatumLookupTable(
        tx: Transaction,
        utxos: Set[Utxo]
    ): LookupTable = {
        val datum = tx
            .getWitnessSet()
            .getPlutusDataList()
            .asScala
            .map: data =>
                ByteString.fromArray(data.getDatumHashAsBytes()) -> data
            .toMap
        // FIXME: implement data in reference inputs
        // FIXME: implement other script types
        val scripts = tx
            .getWitnessSet()
            .getPlutusV2Scripts()
            .asScala
            .map: script =>
                val decoded = Cbor.decode(Hex.hexToBytes(script.getCborHex())).to[Array[Byte]].value
                val flatScript = Cbor.decode(decoded).to[Array[Byte]].value
                ByteString.fromArray(script.getScriptHash) -> (Language.PLUTUS_V2, flatScript)
            .toMap
        LookupTable(scripts, datum)
    }

    def evalPhaseOne(
        tx: Transaction,
        utxos: Set[Utxo],
        lookupTable: LookupTable
    ): Unit = {
        var scripts = scriptsNeeded(tx, utxos);
        validateMissingScripts(scripts, lookupTable);
        hasExactSetOfRedeemers(tx, scripts, lookupTable);
    }

    def scriptsNeeded(tx: Transaction, utxos: Set[Utxo]): List[PlutusScript] = {
        // TODO:
        return List.of();
    }

    def validateMissingScripts(
        scripts: List[PlutusScript],
        lookupTable: LookupTable
    ): Unit = {
        // TODO:
        for (script <- scripts.asScala) {
            if (!lookupTable.scripts.contains(ByteString.fromArray(script.getScriptHash))) {
                throw new IllegalStateException(s"Missing script: ${script.getScriptHash}")
            }
        }
    }

    def hasExactSetOfRedeemers(
        tx: Transaction,
        scripts: List[PlutusScript],
        lookupTable: LookupTable
    ): Unit = {
        // Method body goes here
    }

    def evalRedeemer(
        tx: Transaction,
        utxos: Set[Utxo],
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

        executionPurpose match
            case ExecutionPurpose.WithDatum(scriptVersion, script, datum) =>
                scriptVersion match
                    case Language.PLUTUS_V1 => throw new IllegalStateException("MachineError")
                    case Language.PLUTUS_V2 =>
                        val txInfo = getTxInfoV2(tx, utxos, slotConfig)
                        val scriptContext = v2.ScriptContext(txInfo, purpose)
                        println(s"Script context: $scriptContext")
                        val spender = CountingBudgetSpender()
                        val logger = Log()
                        val cek = new CekMachine(
                          MachineParams.defaultParams,
                          spender,
                          logger,
                          JVMPlatformSpecific
                        )
                        try
                            import scalus.bloxbean.Interop.toScalusData
                            import scalus.builtin.Data.toData
                            import scalus.ledger.api.v2.ToDataInstances.given
                            import scalus.uplc.TermDSL.{*, given}

                            val program = ProgramFlatCodec.decodeFlat(script)
                            val applied = program.term $ toScalusData(datum) $ toScalusData(
                              redeemer.getData
                            ) $ scriptContext.toData

                            val resultTerm = cek.evaluateTerm(applied)
                            val evalResult =
                                CekResult(resultTerm, spender.getSpentBudget, logger.getLogs)
                            val cost = evalResult.budget
                            Redeemer(
                              redeemer.getTag,
                              redeemer.getIndex,
                              redeemer.getData,
                              ExUnits(
                                BigInteger.valueOf(cost.cpu.toLong),
                                BigInteger.valueOf(cost.memory.toLong)
                              )
                            )
                        catch
                            case e: StackTraceMachineError =>
                                println(s"logs: ${logger.getLogs.map(_.toString).mkString("\n")}")
                                println(e.getCekStack.map(_.toString).mkString("\n"))
                                throw new IllegalStateException("MachineError", e)

            case _ => ???

    }

    def getCredential(cred: Credential): v1.Credential = {
        cred.getType match
            case CredentialType.Key =>
                v1.Credential.PubKeyCredential(
                  v1.PubKeyHash(ByteString.fromArray(cred.getBytes))
                )
            case CredentialType.Script =>
                v1.Credential.ScriptCredential(
                  ByteString.fromArray(cred.getBytes)
                )
    }

    def getStakingCredential(cred: StakeCredential): v1.StakingCredential = {
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

    def getAddress(address: Address): v1.Address = {
        println(s"Get address: ${address.getPaymentCredential().get().getType()}")
        val cred = address.getPaymentCredential.map(getCredential).get
        v1.Address(cred, prelude.Maybe.Nothing) // FIXME: Implement staking credential
    }

    def getTxInInfoV2(input: TransactionInput, utxos: Set[Utxo]): v2.TxInInfo = {
        val out = utxos.asScala
            .find(utxo =>
                utxo.getTxHash == input.getTransactionId && utxo.getOutputIndex() == input.getIndex
            )
            .getOrElse(throw new IllegalStateException("Input Not Found"))
        val addr = Address(out.getAddress())
        v2.TxInInfo(
          v1.TxOutRef(
            v1.TxId(ByteString.fromHex(input.getTransactionId)),
            input.getIndex
          ),
          v2.TxOut(
            getAddress(addr),
            cclValueToScalusValue(out.toValue()),
            getOutputDatum(out),
            prelude.Maybe.Nothing
          )
        )
    }

    def cclValueToScalusValue(value: Value): v1.Value = {
        val ma = cclMultiAssetToScalusValue(value.getMultiAssets)
        if value.getCoin != null then
            val lovelace = v1.Value.lovelace(BigInt(value.getCoin))
            prelude.AssocMap(
              prelude.List.Cons(
                (ByteString.empty, AssocMap.singleton(ByteString.empty, BigInt(value.getCoin))),
                ma.inner
              )
            )
        else ma
    }

    def cclMultiAssetToScalusValue(value: List[MultiAsset]): v1.Value = {
        // get sorted multi assets
        val multi = mutable.TreeMap.empty[ByteString, mutable.TreeMap[ByteString, BigInt]]
        for m <- value.asScala do
            val assets = mutable.TreeMap.empty[ByteString, BigInt]
            for asset <- m.getAssets.asScala do
                assets.put(ByteString.fromArray(asset.getNameAsBytes), BigInt(asset.getValue))
            multi.put(ByteString.fromHex(m.getPolicyId), assets)
        // convert to AssocMap
        val am =
            for (policyId, assets) <- multi.iterator
            yield policyId -> AssocMap(prelude.List(assets.toSeq: _*))

        prelude.AssocMap(prelude.List(am.toSeq: _*))
    }

    def getTxOutV2(out: TransactionOutput): v2.TxOut = {
        val addr = Address(out.getAddress())
        v2.TxOut(
          getAddress(addr),
          cclValueToScalusValue(out.getValue()),
          getOutputDatum(out),
          prelude.Maybe.Nothing
        )
    }

    def getOutputDatum(out: TransactionOutput): v2.OutputDatum = {
        if out.getDatumHash != null then
            v2.OutputDatum.OutputDatumHash(ByteString.fromArray(out.getDatumHash))
        else if out.getInlineDatum != null then
            v2.OutputDatum.OutputDatum(toScalusData(out.getInlineDatum))
        else v2.OutputDatum.NoOutputDatum
    }

    def getOutputDatum(out: Utxo): v2.OutputDatum = {
        if out.getDataHash() != null then
            v2.OutputDatum.OutputDatumHash(ByteString.fromHex(out.getDataHash))
        else if out.getInlineDatum != null then
            given Decoder[Data] = PlutusDataCborDecoder
            val data = Cbor.decode(Hex.hexToBytes(out.getInlineDatum)).to[Data].value
            v2.OutputDatum.OutputDatum(data)
        else v2.OutputDatum.NoOutputDatum
    }

    extension [A](inline a: A) inline infix def ??(b: A): A = if a != null then a else b

    def slotToBeginPosixTime(slot: Long, sc: SlotConfig): Long = {
        val msAfterBegin = (slot - sc.zero_slot) * sc.slot_length
        sc.zero_time + msAfterBegin
    }

    def getInterval(tx: Transaction, slotConfig: SlotConfig): v1.Interval[v1.POSIXTime] = {
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

    def getWithdrawals(withdrawals: List[Withdrawal]): AssocMap[v1.StakingCredential, BigInt] = {
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
        AssocMap(prelude.List(wdwls.toSeq: _*))
    }

    def getDCert(cert: Certificate): v1.DCert = {
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

    def getTxInfoV2(
        tx: Transaction,
        utxos: Set[Utxo],
        slotConfig: SlotConfig
    ): v2.TxInfo = {
        val body = tx.getBody
        val certs = body.getCerts ?? List.of()
        v2.TxInfo(
          inputs = prelude.List(
            body.getInputs.asScala.map { input => getTxInInfoV2(input, utxos) }.toSeq: _*
          ),
          referenceInputs = prelude.List(
            body.getReferenceInputs.asScala.map { input => getTxInInfoV2(input, utxos) }.toSeq: _*
          ),
          outputs = prelude.List(body.getOutputs.asScala.map(getTxOutV2).toSeq: _*),
          fee = scalus.ledger.api.v1.Value.lovelace(BigInt(body.getFee ?? BigInteger.ZERO)),
          mint = cclMultiAssetToScalusValue(body.getMint ?? List.of()),
          dcert = prelude.List(certs.asScala.map(getDCert).toSeq: _*),
          withdrawals = getWithdrawals(body.getWithdrawals ?? List.of()),
          validRange = getInterval(tx, slotConfig),
          signatories = prelude.List(
            body.getRequiredSigners.asScala.map { signer =>
                v1.PubKeyHash(ByteString.fromArray(signer))
            }.toList: _*
          ),
          redeemers = AssocMap.empty, // FIXME: Implement redeemers
          data = AssocMap.empty, // FIXME: Implement data
          id = v1.TxId(ByteString.fromHex(TransactionUtil.getTxHash(tx)))
        )
    }

    enum ExecutionPurpose:

        case WithDatum(scriptVersion: Language, script: Array[Byte], datum: PlutusData)

        case NoDatum(scriptVersion: Language, script: Array[Byte])

    def getScriptPurpose(
        redeemer: Redeemer,
        inputs: List[TransactionInput],
        mint: List[MultiAsset],
        certificates: List[Certificate],
        withdrawals: List[Withdrawal]
    ): v1.ScriptPurpose =
        redeemer.getTag match
            case RedeemerTag.Spend =>
                val ins = inputs.asScala.sortBy(_.getIndex)
                val input = ins(redeemer.getIndex.intValue)
                if input != null then
                    v1.ScriptPurpose.Spending(
                      v1.TxOutRef(
                        v1.TxId(ByteString.fromHex(input.getTransactionId)),
                        BigInt(input.getIndex)
                      )
                    )
                else throw new IllegalStateException("ExtraneousRedeemer")
            // FIXME: Implement the rest of the cases
            case _ =>
                throw new IllegalStateException("ExtraneousRedeemer")

    def getExecutionPurpose(
        utxos: Set[Utxo],
        purpose: v1.ScriptPurpose,
        lookupTable: LookupTable
    ): ExecutionPurpose = {
        println(s"Get execution purpose: $purpose, $lookupTable, $utxos")
        purpose match
            case v1.ScriptPurpose.Minting(curSymbol) =>
                ??? // FIXME: Implement minting
            case v1.ScriptPurpose.Spending(txOutRef) =>
                val utxo = utxos.asScala
                    .find(utxo =>
                        ByteString.fromHex(utxo.getTxHash) == txOutRef.id.hash && utxo
                            .getOutputIndex() == txOutRef.idx.toInt
                    )
                    .getOrElse(
                      throw new IllegalStateException("Input Not Found: " + txOutRef)
                    )
                val address = Address(utxo.getAddress())
                println(s"Address: ${address.getPaymentCredential().get().getType()}")
                val hash = ByteString.fromArray(address.getPaymentCredentialHash().orElseThrow())
                val (version, script) = lookupTable.scripts.getOrElse(
                  hash,
                  throw new IllegalStateException("Script Not Found")
                )
                val datumHash = ByteString.fromHex(utxo.getDataHash().nn)
                val datum = lookupTable.datums.getOrElse(
                  datumHash,
                  throw new IllegalStateException("Datum Not Found")
                )
                ExecutionPurpose.WithDatum(version, script, datum)
            case v1.ScriptPurpose.Rewarding(stakingCred) =>
                ??? // FIXME: Implement rewarding
            case v1.ScriptPurpose.Certifying(cert) =>
                ??? // FIXME: Implement certifying
    }

    def evalPhaseTwo(
        tx: Transaction,
        utxos: Set[Utxo],
        costMdls: CostMdls,
        initialBudget: ExBudget,
        slotConfig: SlotConfig,
        runPhaseOne: Boolean
    ): List[Redeemer] =
        println(s"Eval phase two $tx, $utxos, $costMdls, $initialBudget, $slotConfig, $runPhaseOne")
        val redeemers = tx.getWitnessSet.getRedeemers
        val lookupTable = getScriptAndDatumLookupTable(tx, utxos)
        println(s"Lookup table: $lookupTable")

        if runPhaseOne then
            // Subset of phase 1 check on redeemers and scripts
            evalPhaseOne(tx, utxos, lookupTable)

        if redeemers != null && !redeemers.isEmpty() then
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
            }.asJava

            collectedRedeemers
        else List.of()
}
