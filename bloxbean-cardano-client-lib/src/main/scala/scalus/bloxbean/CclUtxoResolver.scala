package scalus.bloxbean

import com.bloxbean.cardano.client.api.UtxoSupplier
import com.bloxbean.cardano.client.api.model.Utxo
import com.bloxbean.cardano.client.exception.CborDeserializationException
import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.spec.Script
import com.bloxbean.cardano.client.transaction.spec.{Transaction, TransactionInput, TransactionOutput, TransactionWitnessSet}
import scalus.utils.Utils

import scala.jdk.CollectionConverters.*
import java.util
import scala.collection.mutable

private[scalus] class CclUtxoResolver(utxoSupplier: UtxoSupplier, scriptSupplier: ScriptSupplier) {
    def resolveUtxos(transaction: Transaction): Map[TransactionInput, TransactionOutput] =
        resolveUtxos(transaction, util.Set.of())

    def resolveUtxos(
        transaction: Transaction,
        inputUtxos: util.Set[Utxo]
    ): Map[TransactionInput, TransactionOutput] = {
        // initialize utxos with inputUtxos
        val utxos = new mutable.HashMap[TransactionInput, Utxo]()
        for utxo <- inputUtxos.asScala do
            val input = TransactionInput.builder
                .transactionId(utxo.getTxHash)
                .index(utxo.getOutputIndex)
                .build
            utxos.put(input, utxo)

        // Get all input utxos using utxoSupplier
        for input <- transaction.getBody.getInputs.asScala do
            if !utxos.contains(input) then
                val utxo = utxoSupplier.getTxOutput(input.getTransactionId, input.getIndex).get
                utxos.put(input, utxo)

        val scripts = new mutable.HashMap[String, Script]()

        // Get all reference inputs utxos including scripts
        for input <- transaction.getBody.getReferenceInputs.asScala do
            val utxo = utxos.getOrElseUpdate(
              input,
              utxoSupplier.getTxOutput(input.getTransactionId, input.getIndex).get
            )
            // Get reference input script
            if utxo.getReferenceScriptHash != null && scriptSupplier != null then
                try
                    val script = scriptSupplier.getScript(utxo.getReferenceScriptHash)
                    scripts.put(utxo.getReferenceScriptHash, script)
                catch
                    case _: Exception =>
                        // this can happen if the Plutus script is not found
                        // usually this is the case for native scripts
                        // we can ignore this
                        ()

        // Initialize witness set to avoid null pointer exceptions
        if transaction.getWitnessSet == null then
            transaction.setWitnessSet(new TransactionWitnessSet)

        val witnessSet = transaction.getWitnessSet
        if witnessSet.getNativeScripts == null then witnessSet.setNativeScripts(new util.ArrayList)

        if witnessSet.getPlutusV1Scripts == null then
            witnessSet.setPlutusV1Scripts(new util.ArrayList)

        if witnessSet.getPlutusV2Scripts == null then
            witnessSet.setPlutusV2Scripts(new util.ArrayList)

        if witnessSet.getPlutusV3Scripts == null then
            witnessSet.setPlutusV3Scripts(new util.ArrayList)

        if witnessSet.getPlutusDataList == null then
            witnessSet.setPlutusDataList(new util.ArrayList)

        // Resolve Utxos
        val resolvedUtxos =
            val witnessScripts = transaction.getWitnessSet.getPlutusV1Scripts.asScala
                ++ transaction.getWitnessSet.getPlutusV2Scripts.asScala
                ++ transaction.getWitnessSet.getPlutusV3Scripts.asScala
                ++ transaction.getWitnessSet.getNativeScripts.asScala

            for s <- witnessScripts do scripts.put(Utils.bytesToHex(s.getScriptHash), s)

            val allInputs =
                transaction.getBody.getInputs.asScala ++
                    transaction.getBody.getReferenceInputs.asScala

            resolveTxInputs(allInputs, utxos, scripts)
        resolvedUtxos
    }

    private def resolveTxInputs(
        inputs: collection.Seq[TransactionInput],
        utxos: collection.Map[TransactionInput, Utxo],
        plutusScripts: collection.Map[String, Script]
    ): Map[TransactionInput, TransactionOutput] = {
        inputs.map { input =>
            val utxo = utxos.getOrElse(
              input,
              throw new IllegalStateException(s"Utxo not found for input $input")
            )
            val scriptOpt = plutusScripts.get(utxo.getReferenceScriptHash)
            val inlineDatum = Option(utxo.getInlineDatum).map(hex =>
                PlutusData.deserialize(Utils.hexToBytes(hex))
            )
            val datumHash = Option(utxo.getDataHash).map(Utils.hexToBytes)
            try
                input -> TransactionOutput.builder
                    .address(utxo.getAddress)
                    .value(utxo.toValue)
                    .datumHash(if inlineDatum.isEmpty then datumHash.orNull else null)
                    .inlineDatum(inlineDatum.orNull)
                    .scriptRef(scriptOpt.orNull)
                    .build
            catch case e: CborDeserializationException => throw new IllegalStateException(e)
        }.toMap
    }

}
