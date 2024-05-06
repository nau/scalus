package scalus.bloxbean

import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.address.CredentialType
import com.bloxbean.cardano.client.api.model.Utxo
import com.bloxbean.cardano.client.exception.CborDeserializationException
import com.bloxbean.cardano.client.exception.CborSerializationException
import com.bloxbean.cardano.client.plutus.spec.*
import com.bloxbean.cardano.client.transaction.spec.*
import com.bloxbean.cardano.client.transaction.spec.cert.Certificate
import com.bloxbean.cardano.client.transaction.util.TransactionUtil
import com.bloxbean.cardano.client.util.HexUtil
import io.bullet.borer.Cbor
import org.checkerframework.checker.units.qual.s
import scalus.builtin.ByteString
import scalus.builtin.ByteString.given
import scalus.builtin.Data
import scalus.builtin.JVMPlatformSpecific
import scalus.builtin.JVMPlatformSpecific$
import scalus.ledger.api.v1
import scalus.ledger.api.v1.Interval
import scalus.ledger.api.v1.ScriptPurpose
import scalus.ledger.api.v1.TxId
import scalus.ledger.api.v1.TxOutRef
import scalus.ledger.api.v2
import scalus.prelude
import scalus.prelude.AssocMap
import scalus.uplc.ProgramFlatCodec
import scalus.uplc.eval.*
import scalus.utils.Hex

import java.awt.image.LookupTable
import java.math.BigInteger
import java.util.*
import java.util.stream.Collectors
import java.util.stream.Stream
import scala.collection.Map
import scala.collection.mutable.ArrayBuffer
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
                            import scalus.builtin.Data.toData
                            import scalus.ledger.api.v2.ToDataInstances.given
                            import scalus.uplc.TermDSL.{*, given}
                            import scalus.bloxbean.Interop.toScalusData

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

    def getAddress(address: Address): v1.Address = {
        println(s"Get address: ${address.getPaymentCredential().get().getType()}")
        val cred = address.getPaymentCredential.map { cred =>
            cred.getType match
                case CredentialType.Key =>
                    v1.Credential.PubKeyCredential(
                      v1.PubKeyHash(ByteString.fromArray(cred.getBytes()))
                    )
                case CredentialType.Script =>
                    v1.Credential.ScriptCredential(
                      ByteString.fromArray(cred.getBytes())
                    )
        }.get

        v1.Address(cred, prelude.Maybe.Nothing) // FIXME: Implement staking credential
    }

    def getTxInInfo(input: TransactionInput, utxos: Set[Utxo]) = {
        val out = utxos.asScala
            .find(utxo =>
                utxo.getTxHash == input.getTransactionId && utxo.getOutputIndex() == input.getIndex
            )
            .getOrElse(throw new IllegalStateException("Input Not Found"))
        val addr = Address(out.getAddress())
        v2.TxInInfo(
          TxOutRef(
            TxId(ByteString.fromHex(input.getTransactionId)),
            input.getIndex
          ),
          v2.TxOut(
            getAddress(addr),
            v1.Value.lovelace(
              BigInt(out.getAmount().get(0).getQuantity())
            ), // FIXME: Handle multiple assets
            v2.OutputDatum.NoOutputDatum, // FIXME: Implement output datum
            prelude.Maybe.Nothing
          )
        )
    }

    // def getTxOut(): TxOut = {}

    def getTxInfoV2(
        tx: Transaction,
        utxos: Set[Utxo],
        slotConfig: SlotConfig
    ): v2.TxInfo = {
        v2.TxInfo(
          inputs = prelude.List(
            tx.getBody().getInputs().asScala.map { input => getTxInInfo(input, utxos) }.toSeq: _*
          ),
          referenceInputs = scalus.prelude.List.Nil, // FIXME: Implement reference inputs
          outputs = scalus.prelude.List.Nil, // FIXME: Implement outputs
          fee = scalus.ledger.api.v1.Value.lovelace(BigInt("188021")), // FIXME: Implement fee
          mint = scalus.ledger.api.v1.Value.lovelace(BigInt("188021")), // FIXME: Implement mint
          dcert = scalus.prelude.List.Nil, // FIXME: Implement dcert
          withdrawals = AssocMap.empty, // FIXME: Implement withdrawals
          validRange = Interval.always, // FIXME: Implement valid range
          signatories = prelude.List(
            tx.getBody()
                .getRequiredSigners()
                .asScala
                .map { signer =>
                    v1.PubKeyHash(ByteString.fromArray(signer))
                }
                .toList: _*
          ),
          redeemers = AssocMap.empty, // FIXME: Implement redeemers
          data = AssocMap.empty, // FIXME: Implement data
          id = TxId(ByteString.fromHex(TransactionUtil.getTxHash(tx)))
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
            // FIXME: Implement the rest of the cases
            case _ =>
                throw new IllegalStateException("ExtraneousRedeemer")

    def getExecutionPurpose(
        utxos: Set[Utxo],
        purpose: ScriptPurpose,
        lookupTable: LookupTable
    ): ExecutionPurpose = {
        println(s"Get execution purpose: $purpose, $lookupTable, $utxos")
        purpose match
            case ScriptPurpose.Minting(curSymbol) =>
                ???
            case ScriptPurpose.Spending(txOutRef) =>
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
            case ScriptPurpose.Rewarding(stakingCred) =>
                ???
            case ScriptPurpose.Certifying(cert) =>
                ???
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
