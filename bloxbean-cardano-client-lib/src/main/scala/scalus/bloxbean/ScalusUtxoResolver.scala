package scalus.bloxbean

import com.bloxbean.cardano.client.api.UtxoSupplier
import com.bloxbean.cardano.client.api.model.Utxo
import com.bloxbean.cardano.client.plutus.spec.{PlutusScript, PlutusV1Script, PlutusV2Script, PlutusV3Script}
import io.bullet.borer.Cbor
import scalus.builtin.ByteString
import scalus.cardano.ledger.*
import scalus.cardano.ledger.BloxbeanToLedgerTranslation.*
import scalus.utils.Hex

import scala.collection.mutable
import scala.jdk.OptionConverters.*
import scala.util.Try

/** Resolves UTXOs for transactions using scalus.cardano.ledger domain classes */
private[scalus] class ScalusUtxoResolver(
    utxoSupplier: UtxoSupplier,
    scriptSupplier: ScriptSupplier
) {

    /** Resolve UTXOs for a transaction with no additional input UTXOs */
    def resolveUtxos(transaction: Transaction): Map[TransactionInput, TransactionOutput] =
        resolveUtxos(transaction, Map.empty)

    /** Resolve UTXOs for a transaction with additional input UTXOs
      *
      * @param transaction
      *   The transaction to resolve UTXOs for
      * @param inputUtxos
      *   Additional UTXOs to include in resolution
      * @return
      *   Map from transaction inputs to their corresponding outputs
      */
    def resolveUtxos(
        transaction: Transaction,
        inputUtxos: Map[TransactionInput, TransactionOutput]
    ): Map[TransactionInput, TransactionOutput] = {
        def decodeToSingleCbor(script: PlutusScript) =
            // unwrap the outer CBOR encoding
            ByteString.unsafeFromArray(
              Cbor.decode(Hex.hexToBytes(script.getCborHex)).to[Array[Byte]].value
            )
        // Initialize UTXOs with provided input UTXOs
        val utxos = mutable.HashMap[TransactionInput, TransactionOutput]()
        inputUtxos.foreach { case (input, output) =>
            utxos.put(input, output)
        }

        val scripts = mutable.HashMap[ScriptHash, Script]()

        // Get all input UTXOs using utxoSupplier
        for input <- transaction.body.value.inputs do
            if !utxos.contains(input) then
                utxoSupplier.getTxOutput(input.transactionId.toHex, input.index).toScala match
                    case Some(output) => utxos.put(input, getTransactionOutput(output))
                    case None => throw new IllegalStateException(s"UTXO not found for input $input")

        // Get all reference input UTXOs including scripts
        for input <- transaction.body.value.referenceInputs do
            if !utxos.contains(input) then
                utxoSupplier.getTxOutput(input.transactionId.toHex, input.index).toScala match
                    case Some(output) =>
                        utxos.put(input, getTransactionOutput(output))
                        // Get reference input script if available
                        Option(output.getReferenceScriptHash).foreach { scriptHash =>
                            Try(scriptSupplier.getScript(scriptHash)).foreach {
                                case s: PlutusV1Script =>
                                    val script = Script.PlutusV1(decodeToSingleCbor(s))
                                    scripts.put(script.scriptHash, script)
                                case s: PlutusV2Script =>
                                    val script = Script.PlutusV2(decodeToSingleCbor(s))
                                    scripts.put(script.scriptHash, script)
                                case s: PlutusV3Script =>
                                    val script = Script.PlutusV3(decodeToSingleCbor(s))
                                    scripts.put(script.scriptHash, script)
                            }
                        }
                    case None =>
                        throw new IllegalStateException(
                          s"Reference UTXO not found for input $input"
                        )

        // Collect witness scripts
        val witnessSet = transaction.witnessSet
        val witnessScripts = witnessSet.nativeScripts ++
            witnessSet.plutusV1Scripts ++
            witnessSet.plutusV2Scripts ++
            witnessSet.plutusV3Scripts

        witnessScripts.foreach { script =>
            scripts.put(script.scriptHash, script)
        }

        // Return resolved UTXOs for all inputs (regular and reference)
        val allInputs = transaction.body.value.inputs ++ transaction.body.value.referenceInputs
        allInputs.map { input =>
            val output = utxos.getOrElse(
              input,
              throw new IllegalStateException(s"UTXO not found for input $input")
            )
            input -> output
        }.toMap
    }
}
