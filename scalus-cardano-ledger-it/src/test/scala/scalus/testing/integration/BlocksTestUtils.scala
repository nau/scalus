package scalus.testing.integration

import co.nstant.in.cbor.{CborException, model as cbor}
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
import scalus.bloxbean.*
import scalus.bloxbean.Interop.??
import scalus.builtin.ByteString
import scalus.utils.Utils

import java.math.BigInteger
import java.nio.channels.FileChannel
import java.nio.file.*
import java.util
import java.util.stream.Collectors
import scala.jdk.CollectionConverters.*

object BlocksTestUtils {

    import com.bloxbean.cardano.yaci.core.config.YaciConfig

    YaciConfig.INSTANCE.setReturnBlockCbor(true) // needed to get the block cbor
    YaciConfig.INSTANCE.setReturnTxBodyCbor(true) // needed to get the tx body cbor

    val apiKey = System.getenv("BLOCKFROST_API_KEY") ?? {
        println("BLOCKFROST_API_KEY is not set, please set it before running the test")
        ""
    }

    val resourcesPath = Paths.get(System.getenv("SCALUS_IT_DATA_PATH")) ?? sys.error(
      "SCALUS_IT_DATA_PATH is not set, please set it before running the test"
    )

    val epochMagic = (System.getenv("SCALUS_IT_EPOCH") ?? "543").toInt

    val backendService = new BFBackendService(Constants.BLOCKFROST_MAINNET_URL, apiKey)
    val utxoSupplier = CachedUtxoSupplier(
      resourcesPath.resolve("utxos"),
      DefaultUtxoSupplier(backendService.getUtxoService)
    )
    val scriptSupplier = InMemoryCachedScriptSupplier(
      FileScriptSupplier(
        resourcesPath.resolve("scripts"),
        ScriptServiceSupplier(backendService.getScriptService)
      )
    )
    val protocolParamsSupplier = CachedEpochParamsSupplier(
      resourcesPath.resolve("epochs"),
      backendService.getEpochService
    )

    val utxoResolver = CclUtxoResolver(utxoSupplier, scriptSupplier)

    def newEvaluator(epoch: Int = epochMagic) = ScalusTransactionEvaluator(
      SlotConfig.Mainnet,
      protocolParamsSupplier.getProtocolParameters(epoch).getValue,
      utxoSupplier,
      scriptSupplier,
      EvaluatorMode.VALIDATE,
      debugDumpFilesForTesting = false
    )

    case class BlockTx(tx: Transaction, datums: java.util.List[ByteString], txHash: String)

    def readTransactionsFromBlockCbor(blockNum: Int): collection.Seq[BlockTx] =
        readTransactionsFromBlockCbor(resourcesPath.resolve(s"blocks/block-$blockNum.cbor"))

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

    def readTransactionsFromBlockCbor(blockCbor: Array[Byte]): collection.Seq[BlockTx] = {
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
            val isValid = !invalidTxs.contains(idx)
            val transaction =
                Transaction
                    .builder()
                    .era(Era.Conway)
                    .body(txbody)
                    .witnessSet(witnessSet)
                    .isValid(isValid)
                    .build()
            val txBodyBytes = tuple._2
            // get tx hash from bytes
            // because Cardano changed TxBody serialization in Conway era, hash(tx.serialized) != hash(txBodyBytes)
            val txHashFromBytes = Utils.bytesToHex(Blake2bUtil.blake2bHash256(txBodyBytes))
            BlockTx(transaction, datumsCbor, txHashFromBytes)
    }

}
