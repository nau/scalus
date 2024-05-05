package scalus.bloxbean

import com.bloxbean.cardano.client.api.model.Utxo;
import com.bloxbean.cardano.client.exception.CborDeserializationException;
import com.bloxbean.cardano.client.exception.CborSerializationException;
import com.bloxbean.cardano.client.plutus.spec.*;
import com.bloxbean.cardano.client.transaction.spec.*;
import com.bloxbean.cardano.client.transaction.spec.cert.Certificate;
import com.bloxbean.cardano.client.util.HexUtil;
import scala.math.BigInt;
import scala.jdk.CollectionConverters._;
import scalus.builtin.ByteString;
import scalus.builtin.JVMPlatformSpecific$;
import scalus.ledger.api.v1.ScriptPurpose;
import scalus.ledger.api.v1.TxId;
import scalus.ledger.api.v1.TxOutRef;
import scalus.ledger.api.v2.ScriptContext;
import scalus.ledger.api.v2.TxInfo;
import scalus.uplc.eval.*;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import scala.collection.mutable.ArrayBuffer
import scalus.utils.Hex
import scalus.builtin.Data
import java.math.BigInteger

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

        val txInputs = transaction.getBody.getInputs
        val refTxInputs = transaction.getBody.getReferenceInputs
        val allInputs =
            Stream.concat(txInputs.stream(), refTxInputs.stream()).collect(Collectors.toList());
        val txOutputs = resolveTxInputs(allInputs, inputUtxos, allScripts)

        try {
            evalPhaseTwo(transaction, allInputs, costMdls, initialBudgetConfig, slotConfig, true)
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

    def getScriptAndDatumLookupTable(
        tx: Transaction,
        utxos: List[TransactionInput]
    ): Map[Array[Byte], String] = {
        return null;
        /*Map<Hash, PlutusData> datum = new HashMap<>();
        Map<Hash, ScriptVersion> scripts = new HashMap<>();

        // Discovery in witness set
        List<PlutusData> plutusDataWitnesses = tx.getTransactionWitnessSet().getPlutusData();
        List<NativeScript> scriptsNativeWitnesses = tx.getTransactionWitnessSet().getNativeScript();
        List<PlutusV1Script> scriptsV1Witnesses = tx.getTransactionWitnessSet().getPlutusV1Script();
        List<PlutusV2Script> scriptsV2Witnesses = tx.getTransactionWitnessSet().getPlutusV2Script();

        for (PlutusData plutusData : plutusDataWitnesses) {
            datum.put(plutusData.originalHash(), plutusData);
        }

        for (NativeScript script : scriptsNativeWitnesses) {
            scripts.put(script.computeHash(), new ScriptVersion.Native(script));
        }

        for (PlutusV1Script script : scriptsV1Witnesses) {
            scripts.put(script.computeHash(), new ScriptVersion.V1(script));
        }

        for (PlutusV2Script script : scriptsV2Witnesses) {
            scripts.put(script.computeHash(), new ScriptVersion.V2(script));
        }

        // Discovery in utxos (script ref)
        for (ResolvedInput utxo : utxos) {
            TransactionOutput output = utxo.getOutput();
            if (output instanceof PostAlonzo) {
                PostAlonzo postAlonzoOutput = (PostAlonzo) output;
                if (postAlonzoOutput.getScriptRef() != null) {
                    PseudoScript pseudoScript = postAlonzoOutput.getScriptRef().getScript();
                    switch (pseudoScript.getVariantCase()) {
                        case NATIVE_SCRIPT:
                            NativeScript ns = pseudoScript.getNativeScript();
                            scripts.put(ns.computeHash(), new ScriptVersion.Native(ns));
                            break;
                        case PLUTUSV1_SCRIPT:
                            PlutusV1Script v1 = pseudoScript.getPlutusV1Script();
                            scripts.put(v1.computeHash(), new ScriptVersion.V1(v1));
                            break;
                        case PLUTUSV2_SCRIPT:
                            PlutusV2Script v2 = pseudoScript.getPlutusV2Script();
                            scripts.put(v2.computeHash(), new ScriptVersion.V2(v2));
                            break;
                        default:
                            break;
                    }
                }
            }
        }

        return new DataLookupTable(datum, scripts);*/
    }

    def evalPhaseOne(
        tx: Transaction,
        utxos: List[TransactionInput],
        lookupTable: Map[Array[Byte], String]
    ): Unit = {
        var scripts = scriptsNeeded(tx, utxos);
        validateMissingScripts(scripts, lookupTable);
        hasExactSetOfRedeemers(tx, scripts, lookupTable);
    }

    def scriptsNeeded(tx: Transaction, utxos: List[TransactionInput]): List[PlutusScript] = {
        // TODO:
        /*pub fn scripts_needed(
    tx: &MintedTx,
    utxos: &[ResolvedInput],
) -> Result<AlonzoScriptsNeeded, Error> {
    let mut needed = Vec::new();

    let txb = tx.transaction_body.clone();

    let mut spend = Vec::new();

    for input in txb.inputs.iter() {
        let utxo = match utxos.iter().find(|utxo| utxo.input == *input) {
            Some(u) => u,
            None => return Err(Error::ResolvedInputNotFound),
        };

        let address = Address::from_bytes(match &utxo.output {
            TransactionOutput::Legacy(output) => output.address.as_ref(),
            TransactionOutput::PostAlonzo(output) => output.address.as_ref(),
        })?;

        if let Address::Shelley(a) = address {
            if let ShelleyPaymentPart::Script(h) = a.payment() {
                spend.push((ScriptPurpose::Spending(input.clone()), *h));
            }
        }
    }

    let mut reward = txb
        .withdrawals
        .as_ref()
        .unwrap_or(&KeyValuePairs::Indef(vec![]))
        .iter()
        .filter_map(|(acnt, _)| {
            let address = Address::from_bytes(acnt).unwrap();

            if let Address::Stake(a) = address {
                if let StakePayload::Script(h) = a.payload() {
                    let cred = StakeCredential::Scripthash(*h);
                    return Some((ScriptPurpose::Rewarding(cred), *h));
                }
            }

            None
        })
        .collect::<AlonzoScriptsNeeded>();

    let mut cert = txb
        .certificates
        .clone()
        .unwrap_or_default()
        .iter()
        .filter_map(|cert| {
            // only Dereg and Deleg certs can require scripts
            match cert {
                Certificate::StakeDeregistration(StakeCredential::Scripthash(h)) => {
                    Some((ScriptPurpose::Certifying(cert.clone()), *h))
                }
                Certificate::StakeDelegation(StakeCredential::Scripthash(h), _) => {
                    Some((ScriptPurpose::Certifying(cert.clone()), *h))
                }
                _ => None,
            }
        })
        .collect::<AlonzoScriptsNeeded>();

    let mut mint = txb
        .mint
        .as_ref()
        .unwrap_or(&KeyValuePairs::Indef(vec![]))
        .iter()
        .map(|(policy_id, _)| (ScriptPurpose::Minting(*policy_id), *policy_id))
        .collect::<AlonzoScriptsNeeded>();

    needed.append(&mut spend);
    needed.append(&mut reward);
    needed.append(&mut cert);
    needed.append(&mut mint);

    Ok(needed)
}*/
        return null;
    }

    def validateMissingScripts(
        scripts: List[PlutusScript],
        lookupTable: Map[Array[Byte], String]
    ): Unit = {
        // TODO:
        for (script <- scripts.asScala) {
            if (!lookupTable.asScala.contains(script.getScriptHash)) {
                throw new IllegalStateException(s"Missing script: ${script.getScriptHash}")
            }
        }
    }

    def hasExactSetOfRedeemers(
        tx: Transaction,
        scripts: List[PlutusScript],
        lookupTable: Map[Array[Byte], String]
    ): Unit = {
        // Method body goes here
    }

    /*
     * pub fn eval_redeemer(
    tx: &MintedTx,
    utxos: &[ResolvedInput],
    slot_config: &SlotConfig,
    redeemer: &Redeemer,
    lookup_table: &DataLookupTable,
    cost_mdls_opt: Option<&CostMdls>,
    initial_budget: &ExBudget,
) -> Result<Redeemer, Error> {
    let result = || {
        let purpose = get_script_purpose(
            redeemer,
            &tx.transaction_body.inputs,
            &tx.transaction_body.mint,
            &tx.transaction_body.certificates,
            &tx.transaction_body.withdrawals,
        )?;

        let execution_purpose: ExecutionPurpose =
            get_execution_purpose(utxos, &purpose, lookup_table)?;

        match execution_purpose {
            ExecutionPurpose::WithDatum(script_version, datum) => match script_version {
                ScriptVersion::V1(script) => {
                    let tx_info = get_tx_info_v1(tx, utxos, slot_config)?;
                    let script_context = ScriptContext { tx_info, purpose };

                    let program: Program<NamedDeBruijn> = {
                        let mut buffer = Vec::new();

                        let prog = Program::<FakeNamedDeBruijn>::from_cbor(&script.0, &mut buffer)?;

                        prog.into()
                    };

                    let program = program
                        .apply_data(datum)
                        .apply_data(redeemer.data.clone())
                        .apply_data(script_context.to_plutus_data());

                    let mut eval_result = if let Some(cost_mdls) = cost_mdls_opt {
                        let costs = if let Some(costs) = &cost_mdls.plutus_v1 {
                            costs
                        } else {
                            return Err(Error::V1CostModelNotFound);
                        };

                        program.eval_as(&Language::PlutusV1, costs, Some(initial_budget))
                    } else {
                        program.eval_version(&Language::PlutusV1)
                    };

                    let cost = eval_result.cost();
                    let logs = eval_result.logs();

                    match eval_result.result() {
                        Ok(_) => (),
                        Err(err) => return Err(Error::Machine(err, cost, logs)),
                    }

                    let new_redeemer = Redeemer {
                        tag: redeemer.tag.clone(),
                        index: redeemer.index,
                        data: redeemer.data.clone(),
                        ex_units: ExUnits {
                            mem: cost.mem as u32,
                            steps: cost.cpu as u64,
                        },
                    };

                    Ok(new_redeemer)
                }
                ScriptVersion::V2(script) => {
                    let tx_info = get_tx_info_v2(tx, utxos, slot_config)?;
                    let script_context = ScriptContext { tx_info, purpose };

                    let program: Program<NamedDeBruijn> = {
                        let mut buffer = Vec::new();

                        let prog = Program::<FakeNamedDeBruijn>::from_cbor(&script.0, &mut buffer)?;

                        prog.into()
                    };

                    let program = program
                        .apply_data(datum)
                        .apply_data(redeemer.data.clone())
                        .apply_data(script_context.to_plutus_data());

                    let mut eval_result = if let Some(cost_mdls) = cost_mdls_opt {
                        let costs = if let Some(costs) = &cost_mdls.plutus_v2 {
                            costs
                        } else {
                            return Err(Error::V2CostModelNotFound);
                        };

                        program.eval_as(&Language::PlutusV2, costs, Some(initial_budget))
                    } else {
                        program.eval(ExBudget::default())
                    };

                    let cost = eval_result.cost();
                    let logs = eval_result.logs();

                    match eval_result.result() {
                        Ok(_) => (),
                        Err(err) => return Err(Error::Machine(err, cost, logs)),
                    }

                    let new_redeemer = Redeemer {
                        tag: redeemer.tag.clone(),
                        index: redeemer.index,
                        data: redeemer.data.clone(),
                        ex_units: ExUnits {
                            mem: cost.mem as u32,
                            steps: cost.cpu as u64,
                        },
                    };

                    Ok(new_redeemer)
                }
                ScriptVersion::Native(_) => Err(Error::NativeScriptPhaseTwo),
            },
            ExecutionPurpose::NoDatum(script_version) => match script_version {
                ScriptVersion::V1(script) => {
                    let tx_info = get_tx_info_v1(tx, utxos, slot_config)?;
                    let script_context = ScriptContext { tx_info, purpose };

                    let program: Program<NamedDeBruijn> = {
                        let mut buffer = Vec::new();

                        let prog = Program::<FakeNamedDeBruijn>::from_cbor(&script.0, &mut buffer)?;

                        prog.into()
                    };

                    let program = program
                        .apply_data(redeemer.data.clone())
                        .apply_data(script_context.to_plutus_data());

                    let mut eval_result = if let Some(cost_mdls) = cost_mdls_opt {
                        let costs = if let Some(costs) = &cost_mdls.plutus_v1 {
                            costs
                        } else {
                            return Err(Error::V1CostModelNotFound);
                        };

                        program.eval_as(&Language::PlutusV1, costs, Some(initial_budget))
                    } else {
                        program.eval_version(&Language::PlutusV1)
                    };

                    let cost = eval_result.cost();
                    let logs = eval_result.logs();

                    match eval_result.result() {
                        Ok(_) => (),
                        Err(err) => return Err(Error::Machine(err, cost, logs)),
                    }

                    let new_redeemer = Redeemer {
                        tag: redeemer.tag.clone(),
                        index: redeemer.index,
                        data: redeemer.data.clone(),
                        ex_units: ExUnits {
                            mem: cost.mem as u32,
                            steps: cost.cpu as u64,
                        },
                    };

                    Ok(new_redeemer)
                }
                ScriptVersion::V2(script) => {
                    let tx_info = get_tx_info_v2(tx, utxos, slot_config)?;
                    let script_context = ScriptContext { tx_info, purpose };

                    let program: Program<NamedDeBruijn> = {
                        let mut buffer = Vec::new();

                        let prog = Program::<FakeNamedDeBruijn>::from_cbor(&script.0, &mut buffer)?;

                        prog.into()
                    };

                    let program = program
                        .apply_data(redeemer.data.clone())
                        .apply_data(script_context.to_plutus_data());

                    let mut eval_result = if let Some(cost_mdls) = cost_mdls_opt {
                        let costs = if let Some(costs) = &cost_mdls.plutus_v2 {
                            costs
                        } else {
                            return Err(Error::V2CostModelNotFound);
                        };

                        program.eval_as(&Language::PlutusV2, costs, Some(initial_budget))
                    } else {
                        program.eval(ExBudget::default())
                    };

                    let cost = eval_result.cost();
                    let logs = eval_result.logs();

                    match eval_result.result() {
                        Ok(_) => (),
                        Err(err) => return Err(Error::Machine(err, cost, logs)),
                    }

                    let new_redeemer = Redeemer {
                        tag: redeemer.tag.clone(),
                        index: redeemer.index,
                        data: redeemer.data.clone(),
                        ex_units: ExUnits {
                            mem: cost.mem as u32,
                            steps: cost.cpu as u64,
                        },
                    };

                    Ok(new_redeemer)
                }
                ScriptVersion::Native(_) => Err(Error::NativeScriptPhaseTwo),
            },
        }
    };

    match result() {
        Ok(r) => Ok(r),
        Err(err) => Err(Error::RedeemerError {
            tag: redeemer_tag_to_string(&redeemer.tag),
            index: redeemer.index,
            err: Box::new(err),
        }),
    }
}*/

    def toScalusData(datum: PlutusData): Data = {
        datum match
            case c: ConstrPlutusData =>
                val constr = c.getAlternative()
                val args = c.getData().getPlutusDataList().asScala.map(toScalusData).toList
                Data.Constr(c.getAlternative(), args)
            case m: MapPlutusData =>
                val values = m
                    .getMap()
                    .asScala
                    .map { case (k, v) => (toScalusData(k), toScalusData(v)) }
                    .toList
                Data.Map(values)
            case l: ListPlutusData =>
                val values = l.getPlutusDataList().asScala.map(toScalusData).toList
                Data.List(values)
            case i: BigIntPlutusData =>
                Data.I(i.getValue())
            case b: BytesPlutusData =>
                Data.B(ByteString.fromArray(b.getValue()))

    }

    def evalRedeemer(
        tx: Transaction,
        utxos: List[TransactionInput],
        slotConfig: SlotConfig,
        redeemer: Redeemer,
        lookupTable: Map[Array[Byte], String],
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
        val executionPurpose = getExecutionPurpose(utxos, purpose, lookupTable)

        executionPurpose match {
            case ExecutionPurpose.WithDatum(scriptVersion, script, datum) =>
                scriptVersion match {
                    case Language.PLUTUS_V1 => throw new IllegalStateException("MachineError")
                    case Language.PLUTUS_V2 =>
                        val txInfo = getTxInfoV2(tx, utxos, slotConfig)
                        val params = MachineParams.defaultParams
                        val scriptContext = ScriptContext(txInfo, purpose)
                        try {
                            import scalus.builtin.Data.toData
                            import scalus.ledger.api.v2.ToDataInstances.given
                            val evalResult = VM.evaluateScriptCounting(
                              params,
                              script,
                              toScalusData(datum),
                              toScalusData(redeemer.getData),
                              scriptContext.toData
                            )
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
                        } catch {
                            case e: StackTraceMachineError =>
                                throw new IllegalStateException("MachineError")
                        }
                }
            case _ => null
        }
    }

    def getTxInfoV2(
        tx: Transaction,
        utxos: List[TransactionInput],
        slotConfig: SlotConfig
    ): TxInfo = {
        // implementation goes here
        null
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
    ): ScriptPurpose =
        redeemer.getTag match
            case RedeemerTag.Spend =>
                val ins = inputs.asScala.sortBy(_.getIndex)
                val input = ins(redeemer.getIndex.intValue)
                if input != null then
                    ScriptPurpose.Spending(
                      TxOutRef(
                        TxId(ByteString.fromHex(input.getTransactionId)),
                        BigInt(input.getIndex)
                      )
                    )
                else throw new IllegalStateException("ExtraneousRedeemer")
            case _ =>
                throw new IllegalStateException("ExtraneousRedeemer")

    def getExecutionPurpose(
        utxos: List[TransactionInput],
        purpose: ScriptPurpose,
        lookupTable: Map[Array[Byte], String]
    ): ExecutionPurpose = {
        // implementation goes here
        null
    }

    def evalPhaseTwo(
        tx: Transaction,
        utxos: List[TransactionInput],
        costMdls: CostMdls,
        initialBudget: ExBudget,
        slotConfig: SlotConfig,
        runPhaseOne: Boolean
    ): List[Redeemer] =
        val redeemers = tx.getWitnessSet.getRedeemers
        val lookupTable = getScriptAndDatumLookupTable(tx, utxos)

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
