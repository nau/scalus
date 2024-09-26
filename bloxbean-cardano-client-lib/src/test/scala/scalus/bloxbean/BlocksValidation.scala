package scalus.bloxbean

import co.nstant.in.cbor.CborException
import co.nstant.in.cbor.model as cbor
import com.bloxbean.cardano.client.backend.api.DefaultUtxoSupplier
import com.bloxbean.cardano.client.backend.blockfrost.common.Constants
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.spec.Era
import com.bloxbean.cardano.client.transaction.spec.*
import com.bloxbean.cardano.client.transaction.util.TransactionUtil
import com.bloxbean.cardano.yaci.core.model.serializers.util.TransactionBodyExtractor
import com.bloxbean.cardano.yaci.core.model.serializers.util.WitnessUtil
import com.bloxbean.cardano.yaci.core.model.serializers.util.WitnessUtil.getArrayBytes
import com.bloxbean.cardano.yaci.core.util.CborSerializationUtil
import scalus.*
import scalus.bloxbean.Interop.??
import scalus.builtin.ByteString
import scalus.utils.Utils

import java.math.BigInteger
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util
import java.util.stream.Collectors
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** Setup BLOCKFROST_API_KEY environment variable before running this test. In SBT shell:
  *   - set `scalus-bloxbean-cardano-client-lib`/envVars := Map("BLOCKFROST_API_KEY" -> "apikey")
  *   - scalus-bloxbean-cardano-client-lib/Test/runMain scalus.bloxbean.BlocksValidation
  *
  *   - cat script-1.flat | uplc evaluate --input-format flat --counting --trace-mode
  *     LogsWithBudgets --builtin-semantics-variant B
  */
object BlocksValidation:

    lazy val apiKey = System.getenv("BLOCKFROST_API_KEY") ?? sys.error(
      "BLOCKFROST_API_KEY is not set, please set it before running the test"
    )

    private def validateBlocksOfEpoch(epoch: Int): Unit = {
        import com.bloxbean.cardano.yaci.core.config.YaciConfig
        YaciConfig.INSTANCE.setReturnBlockCbor(true) // needed to get the block cbor
        YaciConfig.INSTANCE.setReturnTxBodyCbor(true) // needed to get the tx body cbor

        val cwd = Paths.get(".")
        val backendService = new BFBackendService(Constants.BLOCKFROST_MAINNET_URL, apiKey)
        val utxoSupplier = CachedUtxoSupplier(
          cwd.resolve("utxos"),
          DefaultUtxoSupplier(backendService.getUtxoService)
        )
        // memory and file cached script supplier using the script service
        val scriptSupplier = InMemoryCachedScriptSupplier(
          FileScriptSupplier(
            cwd.resolve("scripts"),
            ScriptServiceSupplier(backendService.getScriptService)
          )
        )
        val protocolParams = backendService.getEpochService.getProtocolParameters(epoch).getValue
        val evaluator = ScalusTransactionEvaluator(
          protocolParams,
          utxoSupplier,
          scriptSupplier,
          EvaluatorMode.VALIDATE
        )

        var totalTx = 0
        val errors = mutable.ArrayBuffer[(String, Int, String)]()
        val v1Scripts = mutable.HashSet.empty[String]
        var v1ScriptsExecuted = 0
        val v2Scripts = mutable.HashSet.empty[String]
        var v2ScriptsExecuted = 0
        val v3Scripts = mutable.HashSet.empty[String]
        var v3ScriptsExecuted = 0

        for blockNum <- 10802134 to 10823158 do
            val txs = readTransactionsFromBlockCbor(cwd.resolve(s"blocks/block-$blockNum.cbor"))
//            println(s"Tx hashes: ${txs.map(t => TransactionUtil.getTxHash(t._1)).sorted}")
            val txsWithScripts =
                txs.zipWithIndex
                    .map { case ((tx, datums), idx) =>
                        val utxos = evaluator.resolveUtxos(tx, util.Set.of())
                        val scripts = TxEvaluator.getAllResolvedScripts(tx, utxos)
                        (tx, datums, scripts)
                    }
                    .filter(_._3.nonEmpty)
            println(s"Block $blockNum, num txs to validate: ${txsWithScripts.size}")

            for (tx, datums, scripts) <- txsWithScripts do {
                val txhash = TransactionUtil.getTxHash(tx)
//                println(s"Validating tx $txhash")
                //                println(tx)
                if tx.isValid
                    && (datums.size() == tx.getWitnessSet.getPlutusDataList
                        .size()) // FIXME: remove this check when we have the correct datums
                then
                    val result = evaluator.evaluateTx(tx, util.Set.of(), datums)
                    totalTx += 1
                    if !result.isSuccessful then
                        errors += ((result.getResponse, blockNum, txhash))
                        println(
                          s"${Console.RED}AAAA!!!! block $blockNum $txhash ${result.getResponse}${Console.RESET}"
                        )
                    else
                        println(result.getResponse)
                        for script <- scripts.values do
                            script match
                                case ScriptVersion.PlutusV1(scriptHash) =>
                                    v1Scripts += scriptHash.toHex
                                    v1ScriptsExecuted += 1
                                case ScriptVersion.PlutusV2(scriptHash) =>
                                    v2Scripts += scriptHash.toHex
                                    v2ScriptsExecuted += 1
                                case ScriptVersion.PlutusV3(scriptHash) =>
                                    v3Scripts += scriptHash.toHex
                                    v3ScriptsExecuted += 1
                                case _ =>
                else
                    println(s"${Console.RED}AAAAA invalid!!! $txhash ${Console.RESET}")
                    errors += (("Invalid tx", blockNum, txhash))
            }

//                println("----------------------------------------------------")
            println(s"=======================================")
        println(s"""Total txs: $totalTx,
               |errors: $errors,
               |v1: $v1ScriptsExecuted of ${v1Scripts.size},
               |v2: $v2ScriptsExecuted of ${v2Scripts.size}
               |v3: $v3ScriptsExecuted of ${v3Scripts.size}
               |""".stripMargin)

    }

    def readTransactionsFromBlockCbor(
        path: Path
    ): collection.Seq[(Transaction, util.List[ByteString])] = {
        val blockBytes = Utils.hexToBytes(new String(Files.readAllBytes(path)))
        readTransactionsFromBlockCbor(blockBytes)
    }

    def readTransactionsFromBlockCbor(
        blockCbor: Array[Byte]
    ): collection.Seq[(Transaction, util.List[ByteString])] = {
        val array = CborSerializationUtil.deserializeOne(blockCbor).asInstanceOf[cbor.Array]
        val blockArray = array.getDataItems.get(1).asInstanceOf[cbor.Array]
        val witnessesListArr = blockArray.getDataItems.get(2).asInstanceOf[cbor.Array]
        val auxiliaryDataMap = blockArray.getDataItems.get(3).asInstanceOf[cbor.Map]
        val invalidTxs = blockArray.getDataItems
            .get(4)
            .asInstanceOf[cbor.Array]
            .getDataItems
            .stream()
            .map(_.asInstanceOf[cbor.Number].getValue.intValue())
            .collect(Collectors.toSet())
        val txBodyTuples = TransactionBodyExtractor.getTxBodiesFromBlock(blockCbor)
        val witnesses = WitnessUtil.getWitnessRawData(blockCbor)
        val txBodyDatumsCbor = witnesses.asScala.zipWithIndex
            .map { case (witness, idx) =>
                try
                    val fields = WitnessUtil.getWitnessFields(witness)
                    val datums = fields.get(BigInteger.valueOf(4L))
                    if datums != null then
                        getArrayBytes(datums)
                            .stream()
                            .map(ByteString.fromArray)
                            .collect(Collectors.toList())
                    else util.Collections.emptyList()
                catch
                    case e: CborException =>
                        e.printStackTrace();
                        util.Collections.emptyList();
            }
        for ((tuple, datumsCbor), idx) <- txBodyTuples.asScala.zip(txBodyDatumsCbor).zipWithIndex
        yield
            val txbody = TransactionBody.deserialize(tuple._1.asInstanceOf[cbor.Map])
            val witnessSetDataItem = witnessesListArr.getDataItems.get(idx).asInstanceOf[cbor.Map]
            val witnessSet = TransactionWitnessSet.deserialize(witnessSetDataItem)
            // val auxiliaryData =
            //     if auxiliaryDataMap.get(new UnsignedInteger(idx)) != null then
            //         AuxiliaryData.deserialize(
            //           auxiliaryDataMap.get(new UnsignedInteger(idx)).asInstanceOf[cbor.Map]
            //         )
            //     else null
            val isValid = !invalidTxs.contains(idx)
//            val txbytes =
//                val array = new cbor.Array()
//                array.add(tuple._1)
//                array.add(witnessSetDataItem)
//                array.add(if isValid then SimpleValue.TRUE else SimpleValue.FALSE)
//                array.add(SimpleValue.NULL)
//                CborSerializationUtil.serialize(array)
//            Files.write(Paths.get(s"tx-${TransactionUtil.getTxHash(txbytes)}.cbor"), txbytes)
//            println(Utils.bytesToHex(txbytes))
            /*val witnessMap = witnessesListArr.getDataItems.get(idx).asInstanceOf[cbor.Map]
            val plutusDataArray = witnessMap.get(new UnsignedInteger(4))
            if plutusDataArray != null then
                val plutusData = plutusDataArray.asInstanceOf[cbor.Array]
//                    println(s"size ${plutusData.getDataItems.size()}")
                plutusData.getDataItems.asScala.foreach { item =>
                    // get cbor hex
//                        println(s"${Utils.bytesToHex(CborSerializationUtil.serialize(item))}")
                }*/

            val transaction =
                Transaction
                    .builder()
                    .era(Era.Conway)
                    .body(txbody)
                    .witnessSet(witnessSet)
                    // .auxiliaryData(auxiliaryData)
                    .isValid(isValid)
                    .build()
//            println(s"tx: ${transaction.toJson()}")
//            println(
//              s"txhash ${TransactionUtil.getTxHash(transaction)} ${TransactionUtil.getTxHash(txbytes)}"
//            )
//            println(s"tx ${Utils.bytesToHex(transaction.serialize())}")
            (transaction, datumsCbor)
    }

    def main(args: Array[String]): Unit = {
        validateBlocksOfEpoch(508)
    }
