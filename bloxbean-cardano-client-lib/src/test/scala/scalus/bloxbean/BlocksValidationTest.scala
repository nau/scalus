package scalus.bloxbean

import co.nstant.in.cbor.CborException
import co.nstant.in.cbor.model as cbor
import co.nstant.in.cbor.model.SimpleValue
import co.nstant.in.cbor.model.UnsignedInteger
import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.address.AddressProvider
import com.bloxbean.cardano.client.api.UtxoSupplier
import com.bloxbean.cardano.client.api.common.OrderEnum
import com.bloxbean.cardano.client.api.model.Amount
import com.bloxbean.cardano.client.api.model.Utxo
import com.bloxbean.cardano.client.api.util.CostModelUtil
import com.bloxbean.cardano.client.backend.api.DefaultProtocolParamsSupplier
import com.bloxbean.cardano.client.backend.api.DefaultUtxoSupplier
import com.bloxbean.cardano.client.backend.api.ScriptService
import com.bloxbean.cardano.client.backend.blockfrost.common.Constants
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.backend.blockfrost.service.http.ScriptApi
import com.bloxbean.cardano.client.coinselection.impl.DefaultUtxoSelector
import com.bloxbean.cardano.client.common.ADAConversionUtil
import com.bloxbean.cardano.client.common.CardanoConstants
import com.bloxbean.cardano.client.common.model.Networks
import com.bloxbean.cardano.client.crypto.Blake2bUtil
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.plutus.spec.*
import com.bloxbean.cardano.client.plutus.util.PlutusUtil
import com.bloxbean.cardano.client.quicktx.QuickTxBuilder
import com.bloxbean.cardano.client.quicktx.ScriptTx
import com.bloxbean.cardano.client.spec.Script
import com.bloxbean.cardano.client.transaction.spec.*
import com.bloxbean.cardano.client.transaction.util.TransactionUtil
import com.bloxbean.cardano.yaci.core.model.serializers.util.TransactionBodyExtractor
import com.bloxbean.cardano.yaci.core.model.serializers.util.WitnessUtil
import com.bloxbean.cardano.yaci.core.model.serializers.util.WitnessUtil.getArrayBytes
import com.bloxbean.cardano.yaci.core.util.CborSerializationUtil
import com.fasterxml.jackson.databind.ObjectMapper
import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.builtin.PlutusDataCborDecoder
import scalus.examples.MintingPolicyV2
import scalus.examples.PubKeyValidator
import scalus.prelude.AssocMap
import scalus.uplc.*
import scalus.uplc.TermDSL.*
import scalus.uplc.eval.ExBudget
import scalus.utils.Utils

import java.io.File
import java.math.BigInteger
import java.nio.file.Files
import java.nio.file.Paths
import java.util
import java.util.Optional
import java.util.stream.Collectors
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** Setup BLOCKFROST_API_KEY environment variable before running this test
  */
class BlocksValidationTest extends AnyFunSuite:

    lazy val apiKey = System.getenv("BLOCKFROST_API_KEY")

    test("Validate blocks of epoch 484") {
        import com.bloxbean.cardano.yaci.core.config.YaciConfig
        import com.bloxbean.cardano.yaci.core.model as yaki
        YaciConfig.INSTANCE.setReturnBlockCbor(true) // needed to get the block cbor
        YaciConfig.INSTANCE.setReturnTxBodyCbor(true) // needed to get the tx body cbor

        val backendService = new BFBackendService(Constants.BLOCKFROST_MAINNET_URL, apiKey)
        val utxoSupplier = CachedUtxoSupplier(DefaultUtxoSupplier(backendService.getUtxoService))
        val protocolParamsSupplier =
            new DefaultProtocolParamsSupplier(backendService.getEpochService)
        val scriptSupplier = CachedScriptSupplier(backendService.getScriptService)
        val evaluator =
            ScalusTransactionEvaluator(
              utxoSupplier,
              protocolParamsSupplier,
              scriptSupplier
            )

        var totalTx = 0
        val errors = mutable.ArrayBuffer[String]()
        val v1Scripts = mutable.HashSet.empty[String]
        var v1ScriptsExecuted = 0
        val v2Scripts = mutable.HashSet.empty[String]
        var v2ScriptsExecuted = 0

        for blockNum <- 10310622 to 10310622 do
            val txs = readTransactionsFromBlockCbor(s"blocks/block-$blockNum.cbor")
            val withScriptTxs = txsToEvaluate(txs).drop(1)
            println(s"Block $blockNum, num txs to validate: ${withScriptTxs.size}")

            for (tx, datums) <- withScriptTxs do {
                val txhash = TransactionUtil.getTxHash(tx)
                println(s"Validating tx $txhash")
                //                println(tx)

                Option(tx.getWitnessSet.getPlutusV1Scripts).foreach { scripts =>
                    v1Scripts ++= scripts.asScala.map(s => Utils.bytesToHex(s.getScriptHash))
                    v1ScriptsExecuted += 1
                }
                Option(tx.getWitnessSet.getPlutusV2Scripts).foreach { scripts =>
                    v2Scripts ++= scripts.asScala.map(s => Utils.bytesToHex(s.getScriptHash))
                    v2ScriptsExecuted += 1
                }
                if !tx.isValid then
                    println(
                      s"${Console.RED}AAAAA invalid!!! $txhash ${Console.RESET}"
                    )
//                println(s"datum hashes ${datumHashes.asScala.map(_.toHex)}")
                val result = evaluator.evaluateTx(tx, util.Set.of(), datums)
                totalTx += 1
                if !result.isSuccessful then
                    errors += result.getResponse
                    println(s"${Console.RED}AAAA!!!! $txhash ${result.getResponse}${Console.RESET}")
            }

//                println(tx.getWitnessSet.getRedeemers)
//                println("----------------------------------------------------")
            println(s"=======================================")
        println(
          s"Total txs: $totalTx, errors: $errors, v1: ${v1ScriptsExecuted} of ${v1Scripts.size}, v2: $v2ScriptsExecuted of ${v2Scripts.size}"
        )

    }

    private def txsToEvaluate(txs: collection.Seq[(Transaction, util.List[ByteString])]) = {
        txs.filter { case (tx, _) =>
            tx.getWitnessSet.getPlutusV1Scripts != null || tx.getWitnessSet.getPlutusV2Scripts != null
        }
    }

    def readTransactionsFromBlockCbor(
        filename: String
    ): collection.Seq[(Transaction, util.List[ByteString])] = {
        val blockBytes = Utils.hexToBytes(new String(Files.readAllBytes(Paths.get(filename))))
        readTransactionsFromBlockCbor(blockBytes)
    }

    def readTransactionsFromBlockCbor(
        blockCbor: Array[Byte]
    ): collection.Seq[(Transaction, util.List[ByteString])] = {
        val array = CborSerializationUtil.deserializeOne(blockCbor).asInstanceOf[cbor.Array]
        val blockArray = array.getDataItems.get(1).asInstanceOf[cbor.Array]
        val witnessesListArr = blockArray.getDataItems.get(2).asInstanceOf[cbor.Array]
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
            val txbytes =
                val array = new cbor.Array()
                array.add(tuple._1)
                array.add(witnessSetDataItem)
                array.add(SimpleValue.TRUE)
                array.add(SimpleValue.NULL)
                CborSerializationUtil.serialize(array)
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

            val transaction = Transaction.builder().body(txbody).witnessSet(witnessSet).build()
//            println(
//              s"txhash ${TransactionUtil.getTxHash(transaction)} ${TransactionUtil.getTxHash(txbytes)}"
//            )
//            println(s"tx ${Utils.bytesToHex(transaction.serialize())}")
            (transaction, datumsCbor)
    }

    def validateTransaction(tx: Transaction, evaluator: ScalusTransactionEvaluator): Unit = {}
