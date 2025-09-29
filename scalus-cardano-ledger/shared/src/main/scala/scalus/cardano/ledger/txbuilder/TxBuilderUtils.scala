package scalus.cardano.ledger.txbuilder

import com.bloxbean.cardano.client.api.model.Utxo
import com.bloxbean.cardano.client.plutus.spec.{PlutusScript, PlutusV1Script, PlutusV2Script, PlutusV3Script}
import com.bloxbean.cardano.client.transaction.spec.script.ScriptPubkey
import io.bullet.borer.Cbor
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.utils.Hex.hexToBytes

import scala.collection.mutable

object TxBuilderUtils {

    def resolveUtxos(
        transaction: Transaction,
        context: BuilderContext
    ): Map[TransactionInput, TransactionOutput] =
        resolveUtxos(transaction, Map.empty, context)

    def resolveUtxos(
        transaction: Transaction,
        inputUtxos: Map[TransactionInput, TransactionOutput],
        context: BuilderContext
    ): Map[TransactionInput, TransactionOutput] = {
        def decodeToSingleCbor(script: PlutusScript) =
            // unwrap the outer CBOR encoding
            ByteString.unsafeFromArray(
              Cbor.decode(script.getCborHex.hexToBytes).to[Array[Byte]].value
            )

        // Initialize UTXOs with provided input UTXOs
        val utxos = mutable.HashMap[TransactionInput, TransactionOutput]()
        inputUtxos.foreach { case (input, output) =>
            utxos.put(input, output)
        }

        val scripts = mutable.HashMap[ScriptHash, Script]()

        val allInputs =
            (transaction.body.value.inputs.toSeq ++ transaction.body.value.referenceInputs.toSeq).toSet
        for input <- allInputs do
            if !utxos.contains(input) then
                val outputResult = context.backendService.getUtxoService
                    .getTxOutput(input.transactionId.toHex, input.index)
                val output = outputResult.getValue
                val scriptRef = Option(output).flatMap(o => Option(o.getReferenceScriptHash)).map {
                    scriptHash =>
                        context.backendService.getScriptService
                            .getPlutusScript(scriptHash)
                            .getValue match
                            case s: PlutusV1Script =>
                                ScriptRef(Script.PlutusV1(decodeToSingleCbor(s)))
                            case s: PlutusV2Script =>
                                ScriptRef(Script.PlutusV2(decodeToSingleCbor(s)))
                            case s: PlutusV3Script =>
                                ScriptRef(Script.PlutusV3(decodeToSingleCbor(s)))
                }
                val out = TxBuilderUtils.getTransactionOutput(output, scriptRef)
                utxos.put(input, out)

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
        allInputs.map { input =>
            val output = utxos.getOrElse(
              input,
              throw new IllegalStateException(s"UTXO not found for input $input")
            )
            input -> output
        }.toMap
    }

    def getTransactionOutput(
        output: Utxo,
        scriptRef: Option[ScriptRef]
    ): TransactionOutput = {
        val address = Address.fromBech32(output.getAddress)
        val datumOption: Option[DatumOption] =
            Option(output.getDataHash) -> Option(output.getInlineDatum) match
                case (_, Some(inlineDatum)) =>
                    Some(DatumOption.Inline(Data.fromCbor(inlineDatum.hexToBytes)))
                case (Some(dataHash), None) =>
                    Some(DatumOption.Hash(Hash(ByteString.fromHex(dataHash))))
                case (None, None) => None
        val value = Value.lovelace(output.toValue.getCoin.longValue())
        TransactionOutput(
          address,
          value,
          datumOption,
          scriptRef
        )
    }

    def convertToCCLNativeScript(
        scalusScript: Script.Native
    ): com.bloxbean.cardano.client.transaction.spec.script.NativeScript = {
        import scalus.cardano.ledger.Timelock

        scalusScript.script match {
            case Timelock.Signature(keyHash) =>
                new ScriptPubkey(keyHash.toHex)
            case _ =>
                throw new UnsupportedOperationException(
                  "Only signature-based native scripts are currently supported"
                )
        }
    }
}
