package scalus.bloxbean

import co.nstant.in.cbor.{model as cbor, CborException}
import com.bloxbean.cardano.client.backend.api.DefaultUtxoSupplier
import com.bloxbean.cardano.client.backend.blockfrost.common.Constants
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.crypto.Blake2bUtil
import com.bloxbean.cardano.client.spec.Era
import com.bloxbean.cardano.client.transaction.spec.*
import com.bloxbean.cardano.yaci.core.model.serializers.util.WitnessUtil.getArrayBytes
import com.bloxbean.cardano.yaci.core.model.serializers.util.{TransactionBodyExtractor, WitnessUtil}
import com.bloxbean.cardano.yaci.core.util.CborSerializationUtil
import scalus.*
import scalus.bloxbean.Interop.??
import scalus.bloxbean.TxEvaluator.ScriptHash
import scalus.builtin.{ByteString, PlatformSpecific, given}
import scalus.cardano.ledger.AddrKeyHash
import scalus.ledger.api.{Timelock, ValidityInterval}
import scalus.utils.Utils

import java.math.BigInteger
import java.nio.channels.FileChannel
import java.nio.file.{Path, Paths, StandardOpenOption}
import java.util
import java.util.stream.Collectors
import scala.annotation.unused
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** Setup BLOCKFROST_API_KEY environment variable before running this test. In SBT shell:
  *   - set `scalus-bloxbean-cardano-client-lib`/envVars := Map("BLOCKFROST_API_KEY" -> "apikey")
  *   - scalus-bloxbean-cardano-client-lib/Test/runMain scalus.bloxbean.BlocksValidation
  *   - cat script-1.flat | uplc evaluate --input-format flat --counting --trace-mode
  *     LogsWithBudgets --builtin-semantics-variant B
  */
object BlocksValidation:

    case class BlockTx(tx: Transaction, datums: util.List[ByteString], txHash: String)

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
          SlotConfig.Mainnet,
          protocolParams,
          utxoSupplier,
          scriptSupplier,
          EvaluatorMode.VALIDATE,
          debugDumpFilesForTesting = false
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
            val txsWithScripts =
                val r = mutable.Buffer.empty[
                  (Transaction, util.List[ByteString], String, Map[ScriptHash, ScriptVersion])
                ]
                for BlockTx(tx, datums, txhash) <- txs do
                    try
                        val utxos = evaluator.resolveUtxos(tx, util.Set.of())
                        val scripts = TxEvaluator.getAllResolvedScripts(tx, utxos)
                        if scripts.nonEmpty then r.addOne((tx, datums, txhash, scripts))
                    catch
                        case e: Exception =>
                            println(s"Error in block $blockNum, tx $txhash: ${e.getMessage}")
                r.toSeq
            println(s"Block $blockNum, num txs to validate: ${txsWithScripts.size}")
//            println(s"Block txs:\n${txsWithScripts.map(_._3).sorted.mkString("\n")}")

            for (tx, datums, txhash, scripts) <- txsWithScripts do {
//                println(s"Validating tx $txhash")
                //                println(tx)
                if tx.isValid
                && (datums.size() == tx.getWitnessSet.getPlutusDataList
                    .size()) // FIXME: remove this check when we have the correct datums
                then
                    val result = evaluator.evaluateTx(tx, util.Set.of(), datums, txhash)
                    totalTx += 1
                    if !result.isSuccessful then
                        errors += ((result.getResponse, blockNum, txhash))
                        println(
                          s"${Console.RED}AAAA!!!! block $blockNum $txhash ${result.getResponse}${Console.RESET}"
                        )
                    else
//                        println(result.getResponse)
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

    private def validateNativeScriptOfEpoch(@unused epoch: Int): Unit = {
        import com.bloxbean.cardano.yaci.core.config.YaciConfig
        YaciConfig.INSTANCE.setReturnBlockCbor(true) // needed to get the block cbor
        YaciConfig.INSTANCE.setReturnTxBodyCbor(true) // needed to get the tx body cbor

        val cwd = Paths.get(".")
        case class Res(var succ: Int, var fail: Int)
        val stats = mutable.HashMap.empty[ByteString, Res].withDefaultValue(Res(0, 0))
        val start = System.currentTimeMillis()
        for blockNum <- 11544518 to 11662495 do
            val txs = readTransactionsFromBlockCbor(cwd.resolve(s"blocks/block-$blockNum.cbor"))
            for BlockTx(tx, datums, txhash) <- txs do
                try
                    if tx.getWitnessSet.getNativeScripts ne null then
                        for script <- tx.getWitnessSet.getNativeScripts.asScala do
                            val scriptHash = ByteString.fromArray(script.getScriptHash)
                            val timelock = Timelock.fromCbor(
                              script.serialize().drop(1)
                            ) // drop NativeScript tag
                            val keyHashes =
                                val hashes = for
                                    ws <- Option(tx.getWitnessSet)
                                    wit <- Option(ws.getVkeyWitnesses)
                                yield for w <- wit.asScala yield
                                    val key = ByteString.fromArray(w.getVkey)
                                    AddrKeyHash(summon[PlatformSpecific].blake2b_224(key))
                                hashes.map(_.toSet).getOrElse(Set.empty)

                            if timelock.evaluate(
                                  keyHashes,
                                  ValidityInterval(
                                    Some(tx.getBody.getValidityStartInterval),
                                    Some(tx.getBody.getTtl)
                                  )
                                )
                            then stats.getOrElseUpdate(scriptHash, Res(0, 0)).succ += 1
                            else stats.getOrElseUpdate(scriptHash, Res(0, 0)).fail += 1
                catch
                    case e: Exception =>
                        println(s"Error in block $blockNum, tx $txhash: ${e.getMessage}")
            println(s"Block $blockNum")
        end for
        println(s"Time taken: ${System.currentTimeMillis() - start} ms")
        println(
          s"Stats: num scripts ${stats.size}, succ: ${stats.values.map(_.succ).sum}, failed: ${stats.values.map(_.fail).sum}"
        )
    }

    def readTransactionsFromBlockCbor(path: Path): collection.Seq[BlockTx] = {
        // read block cbor from file using mmap
        val channel = FileChannel.open(path, StandardOpenOption.READ)
        try
            val size = channel.size()
            val buffer = channel.map(FileChannel.MapMode.READ_ONLY, 0, size)
            val output = new Array[Byte](size.toInt)
            buffer.get(output)
            readTransactionsFromBlockCbor(output)
        finally channel.close()
    }

    def readTransactionsFromBlockCbor(
        blockCbor: Array[Byte]
    ): collection.Seq[BlockTx] = {
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
        val txBodyDatumsCbor = witnesses.asScala
            .map { witness =>
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
            val txBodyBytes = tuple._2
            // get tx hash from bytes
            // because Cardano changed TxBody serialization in Conway era, hash(tx.serialized) != hash(txBodyBytes)
            val txHashFromBytes = Utils.bytesToHex(Blake2bUtil.blake2bHash256(txBodyBytes))
            BlockTx(transaction, datumsCbor, txHashFromBytes)
    }

    def main(args: Array[String]): Unit = {
//        validateBlocksOfEpoch(508)
        validateNativeScriptOfEpoch(508)
    }
