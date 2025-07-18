package scalus.bloxbean

import co.nstant.in.cbor.{model as cbor, CborException}
import com.bloxbean.cardano.client.api.UtxoSupplier
import com.bloxbean.cardano.client.api.util.CostModelUtil.{PlutusV1CostModel, PlutusV2CostModel, PlutusV3CostModel}
import com.bloxbean.cardano.client.backend.api.DefaultUtxoSupplier
import com.bloxbean.cardano.client.backend.blockfrost.common.Constants
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.crypto.Blake2bUtil
import com.bloxbean.cardano.client.plutus.spec.CostMdls
import com.bloxbean.cardano.client.spec.Era
import com.bloxbean.cardano.client.transaction.spec.*
import com.bloxbean.cardano.yaci.core.model.serializers.util.WitnessUtil.getArrayBytes
import com.bloxbean.cardano.yaci.core.model.serializers.util.{TransactionBodyExtractor, WitnessUtil}
import com.bloxbean.cardano.yaci.core.util.CborSerializationUtil
import scalus.*
import scalus.bloxbean.Interop.??
import scalus.bloxbean.TxEvaluator.ScriptHash
import scalus.builtin.{platform, ByteString}
import scalus.cardano.ledger
import scalus.cardano.ledger.{AddrKeyHash, BlockFile, Hash, Language, Script, ScriptDataHashGenerator}
import scalus.ledger.api.ValidityInterval
import scalus.ledger.babbage.ProtocolParams
import scalus.utils.Hex.toHex
import scalus.utils.Utils
import upickle.default.read

import java.math.BigInteger
import java.nio.channels.FileChannel
import java.nio.file.{Files, Path, Paths, StandardCopyOption, StandardOpenOption}
import java.util
import java.util.stream.Collectors
import scala.collection.immutable.TreeSet
import scala.collection.{immutable, mutable}
import scala.jdk.CollectionConverters.*
import scala.util.Using

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
        val utxoResolver = CclUtxoResolver(utxoSupplier, scriptSupplier)
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

        for blockNum <- 11544518 to 11546100 do
            val txs = readTransactionsFromBlockCbor(cwd.resolve(s"blocks/block-$blockNum.cbor"))
            val txsWithScripts =
                val r = mutable.Buffer.empty[
                  (Transaction, util.List[ByteString], String, Map[ScriptHash, Script])
                ]
                for BlockTx(tx, datums, txhash) <- txs do
                    try
                        val utxos = utxoResolver.resolveUtxos(tx)
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
                                case _: Script.PlutusV1 =>
                                    v1Scripts += script.scriptHash.toHex
                                    v1ScriptsExecuted += 1
                                case _: Script.PlutusV2 =>
                                    v2Scripts += script.scriptHash.toHex
                                    v2ScriptsExecuted += 1
                                case _: Script.PlutusV3 =>
                                    v3Scripts += script.scriptHash.toHex
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

    private def getAllBlocks(): IndexedSeq[Path] = {
        val cwd = Paths.get(".")
        val blocksDir = cwd.resolve("blocks")
        if !Files.exists(blocksDir) then
            sys.error(
              s"Blocks directory $blocksDir does not exist. Please run `sbt bloxbean-cardano-client-lib/test` first."
            )
        Using(Files.list(blocksDir)) {
            _.filter(f => f.getFileName.toString.endsWith(".cbor"))
                .iterator()
                .asScala
                .toIndexedSeq
                .sorted
        }.get
    }

    @main
    def validateNativeScriptEvaluation(): Unit = {
        case class Res(
            var succ: Int,
            var fail: Int,
            blocks: mutable.ArrayBuffer[Int] = mutable.ArrayBuffer.empty
        )

        val stats = mutable.HashMap.empty[ByteString, Res].withDefaultValue(Res(0, 0))
        val start = System.currentTimeMillis()

        val blocks = getAllBlocks()

        for path <- blocks do
            try
                val blockBytes = Files.readAllBytes(path)
                val block = BlockFile.fromCborArray(blockBytes).block
                for
                    (txb, w) <- block.transactionBodies.zip(block.transactionWitnessSets)
                    native <- w.nativeScripts
                do
                    val scriptHash = native.scriptHash
                    val keyHashes = w.vkeyWitnesses.map { w =>
                        val key = w.vkey
                        AddrKeyHash(platform.blake2b_224(key))
                    }

                    if native.script.evaluate(
                          keyHashes,
                          ValidityInterval(txb.validityStartSlot, txb.ttl)
                        )
                    then stats.getOrElseUpdate(scriptHash, Res(0, 0)).succ += 1
                    else stats.getOrElseUpdate(scriptHash, Res(0, 0)).fail += 1

            catch
                case e: Exception =>
                    println(s"Error reading block $path: ${e.getMessage}")
                    e.printStackTrace()
            print(s"\rBlock $path")
        end for
        println()
        println(s"Time taken: ${System.currentTimeMillis() - start} ms")
        println(
          s"Stats: num scripts ${stats.size}, succ: ${stats.values.map(_.succ).sum}, failed: ${stats.values.map(_.fail).sum}"
        )
    }

    @main
    def validateScriptDataHashEvaluation(): Unit = {
        import com.bloxbean.cardano.yaci.core.config.YaciConfig
        YaciConfig.INSTANCE.setReturnBlockCbor(true) // needed to get the block cbor
        YaciConfig.INSTANCE.setReturnTxBodyCbor(true) // needed to get the tx body cbor
        case class Res(
            var succ: Int,
            var fail: Int,
            blocks: mutable.ArrayBuffer[Int] = mutable.ArrayBuffer.empty
        )

        val cwd = Paths.get(".")
        val blocksDir = cwd.resolve("blocks")
        val stats = mutable.HashMap.empty[ByteString, Res].withDefaultValue(Res(0, 0))
        val start = System.currentTimeMillis()

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

        val params: ProtocolParams = read[ProtocolParams](
          this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
        )(using ProtocolParams.blockfrostParamsRW)

        val blocks = getAllBlocks()

        for path <- blocks do
            try
                val blockBytes = Files.readAllBytes(path)
                val bbTxs = readTransactionsFromBlockCbor(blockBytes)
                val block = BlockFile.fromCborArray(blockBytes).block
                for (((txb, w), bbtx), idx) <- block.transactionBodies
                        .zip(block.transactionWitnessSets)
                        .zip(bbTxs)
                        .zipWithIndex
                do
//                    pprint.pprintln(txb)
//                    pprint.pprintln(w)
                    txb.scriptDataHash match

                        case Some(scriptDataHash) =>
                            val refScriptTypes =
                                getRefScriptTypes(utxoSupplier, scriptSupplier, txb)

                            val costModels =
                                ScriptDataHashGenerator.getUsedCostModels(params, w, refScriptTypes)
                            val calculatedHash = ScriptDataHashGenerator.computeScriptDataHash(
                              ledger.Era.Conway,
                              w.redeemers,
                              w.plutusData,
                              costModels
                            )

                            val bbgenerated: Array[Byte] = generateBloxBeanHash(bbtx, bbtx.tx)

                            val desc =
                                (if w.plutusV1Scripts.nonEmpty then "v1" else "")
                                    ++ (if w.plutusV2Scripts.nonEmpty then "v2" else "")
                                    ++ (if w.plutusV3Scripts.nonEmpty then "v3" else "")
                                    ++ (if w.plutusData.value.toIndexedSeq.nonEmpty then "D"
                                        else "")
                                    ++ (if w.redeemers.nonEmpty then "R" else "")

                            val sameAsBloxbean =
                                calculatedHash.toHex == bbgenerated.toHex // at least same as bloxbean
                            val scalusHasCorrectHash =
                                scriptDataHash.toHex == calculatedHash.toHex // mine is correct
                            val color =
                                if scalusHasCorrectHash then Console.GREEN
                                else if sameAsBloxbean then Console.YELLOW
                                else Console.RED

                            if !scalusHasCorrectHash then
                                println(
                                  s"$idx: $desc ${color}data hash: ${scriptDataHash.toHex}, calculated: ${calculatedHash.toHex} " +
                                      s"bbgen: ${bbgenerated.toHex}${Console.RESET}"
                                )
                        case _ =>

            catch
                case e: Exception =>
                    println(s"Error reading block $path: ${e.getMessage}")
                    e.printStackTrace()
            print(s"\rBlock $path")
        end for
        println()
        println(s"Time taken: ${System.currentTimeMillis() - start} ms")
        println(
          s"Stats: num scripts ${stats.size}, succ: ${stats.values.map(_.succ).sum}, failed: ${stats.values.map(_.fail).sum}"
        )
    }

    private def getRefScriptTypes(
        utxoSupplier: UtxoSupplier,
        scriptSupplier: ScriptSupplier,
        txb: ledger.TransactionBody
    ): TreeSet[Language] = {
        import scala.jdk.OptionConverters.RichOptional
        val refScripts = txb.referenceInputs.view
            .flatMap { refInputs =>
                utxoSupplier
                    .getTxOutput(refInputs.transactionId.toHex, refInputs.index)
                    .toScala
                    .flatMap(utxo => Option(utxo.getReferenceScriptHash))
                    .map { refScriptHash =>
                        Hash.scriptHash(ByteString.fromHex(refScriptHash))
                    }
            }
            .map { refScriptHash =>
                scriptSupplier.getScript(refScriptHash.toHex)
            }
            .toSet
        val refScriptTypes = refScripts
            .map(s => Language.fromId(s.getLanguage.getKey))
            .to(TreeSet)
        refScriptTypes
    }

    private def generateBloxBeanHash(bbtx: BlockTx, transaction: Transaction) = {
        val costMdls = new CostMdls()

        if transaction.getWitnessSet.getPlutusV1Scripts != null && transaction.getWitnessSet.getPlutusV1Scripts.size > 0
        then {
            costMdls.add(PlutusV1CostModel)
        }

        if transaction.getWitnessSet.getPlutusV2Scripts != null && transaction.getWitnessSet.getPlutusV2Scripts.size > 0
        then {
            costMdls.add(PlutusV2CostModel)
        }

        if transaction.getWitnessSet.getPlutusV3Scripts != null && transaction.getWitnessSet.getPlutusV3Scripts.size > 0
        then {
            costMdls.add(PlutusV3CostModel)
        }

        import com.bloxbean.cardano.client.plutus.util.ScriptDataHashGenerator.generate
        val bbgenerated = generate(
          Era.Conway,
          bbtx.tx.getWitnessSet.getRedeemers,
          bbtx.tx.getWitnessSet.getPlutusDataList,
          costMdls
        )
        bbgenerated
    }

    @main
    def findInterestingBlocks(): Unit = {
        val blocks = getAllBlocks()
        println(s"Found ${blocks.size} blocks")
        val interestingBlocks = blocks.filter { path =>
            val blockBytes = Files.readAllBytes(path)
            val block = BlockFile.fromCborArray(blockBytes).block
            block.transactionWitnessSets.exists { _.plutusV1Scripts.nonEmpty } &&
            block.transactionWitnessSets.exists { _.plutusV2Scripts.nonEmpty } &&
            block.transactionWitnessSets.exists { _.plutusV3Scripts.nonEmpty } &&
            block.transactionWitnessSets.exists { _.nativeScripts.nonEmpty } &&
            block.transactionWitnessSets.exists { _.vkeyWitnesses.nonEmpty } &&
            block.transactionWitnessSets.exists { _.plutusData.value.toIndexedSeq.nonEmpty }
        }
        println(s"Interesting blocks ${interestingBlocks.size} of ${blocks.size}")
        interestingBlocks.foreach { p =>
            // copy  to resources
            Files.copy(
              p,
              Paths.get("src", "test", "resources", p.getFileName.toString),
              StandardCopyOption.REPLACE_EXISTING
            )
        }
    }

    @main
    def validateBlocks(): Unit = {
        validateBlocksOfEpoch(543)
    }

    def main(args: Array[String]): Unit = {
        validateBlocksOfEpoch(543)
        validateNativeScriptEvaluation()
        validateScriptDataHashEvaluation()
    }
