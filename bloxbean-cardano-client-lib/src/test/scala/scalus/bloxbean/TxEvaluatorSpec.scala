package scalus.bloxbean

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
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.plutus.spec.*
import com.bloxbean.cardano.client.plutus.util.PlutusUtil
import com.bloxbean.cardano.client.quicktx.QuickTxBuilder
import com.bloxbean.cardano.client.quicktx.ScriptTx
import com.bloxbean.cardano.client.spec.Script
import com.bloxbean.cardano.client.transaction.spec.*
import com.bloxbean.cardano.client.transaction.util.TransactionUtil
import com.bloxbean.cardano.yaci.core.model.serializers.TransactionBodyExtractor
import com.bloxbean.cardano.yaci.core.util.CborSerializationUtil
import com.fasterxml.jackson.databind.ObjectMapper
import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.ByteString
import scalus.builtin.ByteString.given
import scalus.builtin.Data
import scalus.examples.MintingPolicyV2
import scalus.examples.PubKeyValidator
import scalus.prelude.AssocMap
import scalus.uplc.*
import scalus.uplc.TermDSL.{*, given}
import scalus.uplc.eval.ExBudget
import scalus.utils.Utils

import java.io.File
import java.math.BigInteger
import java.nio.file.Files
import java.nio.file.Paths
import java.util
import java.util.Optional
import scala.collection.mutable

class TxEvaluatorSpec extends AnyFunSuite:
    val senderMnemonic: String =
        "drive useless envelope shine range ability time copper alarm museum near flee wrist live type device meadow allow churn purity wisdom praise drop code";
    val sender1 = new Account(Networks.testnet(), senderMnemonic)
    val sender1Addr: String = sender1.baseAddress()

    test("TxEvaluator ") {
        import scala.jdk.CollectionConverters.*
        val costMdls = CostMdls()
        costMdls.add(CostModelUtil.PlutusV1CostModel)
        costMdls.add(CostModelUtil.PlutusV2CostModel)
        val evaluator = TxEvaluator(
          SlotConfig.default,
          initialBudgetConfig = ExBudget.fromCpuAndMemory(10_000000000L, 10_000000L),
          protocolMajorVersion = 8,
          costMdls = costMdls
        )
        val pubKeyValidator =
            compile(PubKeyValidator.validatorV2(hex"deadbeef")).toPlutusProgram((1, 0, 0))
        val s: PlutusV2Script =
            PlutusV2Script
                .builder()
                .cborHex(pubKeyValidator.doubleCborHex)
                .build()
                .asInstanceOf[PlutusV2Script]
        val scripts: util.List[PlutusScript] = util.List.of(s)
        val pubKeyScriptAddress = AddressProvider.getEntAddress(s, Networks.testnet())
        println(
          s"Pubkey script address: ${pubKeyScriptAddress.getAddress}, type hash: ${pubKeyScriptAddress.getPaymentCredentialHash
                  .map(ByteString.fromArray)}"
        )

        val input = TransactionInput.builder().transactionId("deadbeef").index(0).build()
        val inputs = util.List.of(input)

        val utxo = Map(
          input -> TransactionOutput
              .builder()
              .value(Value.builder().coin(BigInteger.valueOf(20)).build())
              .address(pubKeyScriptAddress.getAddress)
              .datumHash(Utils.hexToBytes(PlutusData.unit().getDatumHash))
              .build()
        )
        val redeemer = Redeemer
            .builder()
            .tag(RedeemerTag.Spend)
            .data(PlutusData.unit())
            .index(BigInteger.ZERO)
            .exUnits(
              ExUnits
                  .builder()
                  .mem(BigInteger.valueOf(0L))
                  .steps(BigInteger.valueOf(0L))
                  .build()
            )
            .build()
        val tx = Transaction
            .builder()
            .body(
              TransactionBody
                  .builder()
                  .fee(ADAConversionUtil.adaToLovelace(0.2))
                  .ttl(1000)
                  .inputs(inputs)
                  .requiredSigners(util.List.of(hex"deadbeef".bytes))
                  .build()
            )
            .witnessSet(
              TransactionWitnessSet
                  .builder()
                  .plutusV2Scripts(util.List.of(s))
                  .redeemers(util.List.of(redeemer))
                  .plutusDataList(util.List.of(PlutusData.unit()))
                  .build()
            )
            .build()
        val redeemers = evaluator.evaluateTx(tx, utxo)
        println(redeemers)
    }

    test("asdf") {
        val scriptcbor =
            "59014f59014c01000032323232323232322223232325333009300e30070021323233533300b3370e9000180480109118011bae30100031225001232533300d3300e22533301300114a02a66601e66ebcc04800400c5288980118070009bac3010300c300c300c300c300c300c300c007149858dd48008b18060009baa300c300b3754601860166ea80184ccccc0288894ccc04000440084c8c94ccc038cd4ccc038c04cc030008488c008dd718098018912800919b8f0014891ce1317b152faac13426e6a83e06ff88a4d62cce3c1634ab0a5ec133090014a0266008444a00226600a446004602600a601a00626600a008601a006601e0026ea8c03cc038dd5180798071baa300f300b300e3754601e00244a0026eb0c03000c92616300a001375400660106ea8c024c020dd5000aab9d5744ae688c8c0088cc0080080048c0088cc00800800555cf2ba15573e6e1d200201"
        val scriptFlat = Cbor
            .decode(Cbor.decode(Utils.hexToBytes(scriptcbor)).to[Array[Byte]].value)
            .to[Array[Byte]]
            .value
        val program = ProgramFlatCodec.decodeFlat(scriptFlat)
        val namedTerm = DeBruijn.fromDeBruijnTerm(program.term)
        println(namedTerm.prettyXTerm.render(120))
    }

    class CachedUtxoSupplier(default: UtxoSupplier) extends UtxoSupplier {
        val cache = mutable.HashMap[(String, Int), Utxo]()
        override def getPage(
            address: String,
            nrOfItems: Integer,
            page: Integer,
            order: OrderEnum
        ): util.List[Utxo] = default.getPage(address, nrOfItems, page, order)

        val objectMapper = new ObjectMapper()
        override def getTxOutput(
            txHash: String,
            outputIndex: Int
        ): Optional[Utxo] = {
            if cache.contains((txHash, outputIndex)) then Optional.of(cache((txHash, outputIndex)))
            else if Files.exists(Paths.get(s"utxos/$txHash-$outputIndex")) then
//                println(s"found $txHash-$outputIndex in utxos folder")
                val utxo =
                    objectMapper.readValue(new File(s"utxos/$txHash-$outputIndex"), classOf[Utxo])
                cache.put((txHash, outputIndex), utxo)
                Optional.of(utxo)
            else
                val utxo = default.getTxOutput(txHash, outputIndex)
                utxo.ifPresent({ u =>
                    cache.put((txHash, outputIndex), u)
                    objectMapper.writeValue(new File(s"utxos/$txHash-$outputIndex"), u)
//                    println(s"queried $txHash-$outputIndex in blockfrost and saved to utxos folder")
                })
                utxo
        }
    }

    class CachedScriptSupplier(scriptService: ScriptService) extends ScriptSupplier {
        private val cache = mutable.Map[String, PlutusScript]()
        override def getScript(scriptHash: String): PlutusScript = {
            //                    println(s"getting script $scriptHash")
            if cache.contains(scriptHash) then cache(scriptHash)
            else if Files.exists(Paths.get(s"scripts/$scriptHash")) then
//                println(s"found $scriptHash in scripts folder")
                val scriptBytes = Files.readAllBytes(Paths.get(s"scripts/$scriptHash"))
                val script =
                    PlutusUtil.getPlutusScript(scriptHash, Utils.bytesToHex(scriptBytes)).get
                cache.put(scriptHash, script)
                script
            else
                val script = scriptService.getPlutusScript(scriptHash)
                if script.isSuccessful then
                    val s = script.getValue
                    cache.put(scriptHash, s)
                    Files.write(Paths.get(s"scripts/$scriptHash"), Utils.hexToBytes(s.getCborHex))
//                    println(s"queried $scriptHash in blockfrost and saved to scripts folder")
                    s
                else throw new RuntimeException(s"Script not found $scriptHash")
        }
    }

    test("Validate blocks of epoch 484") {
        import com.bloxbean.cardano.yaci.core.config.YaciConfig
        import com.bloxbean.cardano.yaci.core.model as yaki
        import scala.jdk.CollectionConverters.*
        import co.nstant.in.cbor.model as cbor

        YaciConfig.INSTANCE.setReturnBlockCbor(true) // needed to get the block cbor
        YaciConfig.INSTANCE.setReturnTxBodyCbor(true) // needed to get the tx body cbor
        val apiKey = System.getenv("BLOCKFROST_API_KEY")
        val backendService = new BFBackendService(Constants.BLOCKFROST_MAINNET_URL, apiKey)
        val utxoSupplier = CachedUtxoSupplier(DefaultUtxoSupplier(backendService.getUtxoService))
        val protocolParamsSupplier =
            new DefaultProtocolParamsSupplier(backendService.getEpochService)
        val scriptSupplier = CachedScriptSupplier(backendService.getScriptService)
        val evaluator =
            ScalusTransactionEvaluator(
              utxoSupplier,
              protocolParamsSupplier,
              scriptSupplier,
              debugStoreInsOuts = false
            )

        def readTransactionsFromBlockCbor(blockCbor: Array[Byte]): collection.Seq[Transaction] = {
            val array = CborSerializationUtil.deserializeOne(blockCbor).asInstanceOf[cbor.Array]
            val blockArray = array.getDataItems.get(1).asInstanceOf[cbor.Array]
            val witnessesListArr = blockArray.getDataItems.get(2).asInstanceOf[cbor.Array]
            val txBodyTuples = TransactionBodyExtractor.getTxBodiesFromBlock(blockCbor)
            for (tuple, idx) <- txBodyTuples.asScala.zipWithIndex yield
                val txbody = TransactionBody.deserialize(tuple._1.asInstanceOf[cbor.Map])
                val witnessSet = TransactionWitnessSet.deserialize(
                  witnessesListArr.getDataItems.get(idx).asInstanceOf[cbor.Map]
                )
                /*val witnessMap = witnessesListArr.getDataItems.get(idx).asInstanceOf[cbor.Map]
                val plutusDataArray = witnessMap.get(new UnsignedInteger(4))
                if plutusDataArray != null then
                    val plutusData = plutusDataArray.asInstanceOf[cbor.Array]
//                    println(s"size ${plutusData.getDataItems.size()}")
                    plutusData.getDataItems.asScala.foreach { item =>
                        // get cbor hex
//                        println(s"${Utils.bytesToHex(CborSerializationUtil.serialize(item))}")
                    }*/

                Transaction.builder().body(txbody).witnessSet(witnessSet).build()
        }
        var totalTx = 0
        var errors = mutable.ArrayBuffer[String]()
        var v1Scripts = mutable.HashSet.empty[String]
        var v2Scripts = mutable.HashSet.empty[String]

        for blockNum <- 10295004 to 10295004 do
            val blockCbor =
                new String(Files.readAllBytes(Paths.get(s"blocks/block-$blockNum.cbor")))
            val blockBytes = Utils.hexToBytes(blockCbor)
            val txs = readTransactionsFromBlockCbor(blockBytes)
            val withScriptTxs =
                txs.filter(tx =>
                    tx.getWitnessSet.getPlutusV1Scripts != null || tx.getWitnessSet.getPlutusV2Scripts != null
                )
            println(s"Block $blockNum, num txs to validate: ${withScriptTxs.size}")

            /*
             * Run cargo run -- tx simulate --cbor ~/projects/scalus/bloxbean-cardano-client-lib/tx-e1d7afe71bfb3716916889e8c4946c5aa5b9151ca8d0baadc38f3c780d813d22.cbor ~/projects/scalus/bloxbean-cardano-client-lib/ins.cbor ~/projects/scalus/bloxbean-cardano-client-lib/outs.cbor
             */

            for tx <- withScriptTxs do
                println(s"Validating tx ${TransactionUtil.getTxHash(tx)}")
//                tx.getWitnessSet.getPlutusDataList.get(0).getDatumHash
//                println(tx)
//                Files.write(Paths.get(s"tx-${TransactionUtil.getTxHash(tx)}.cbor"), tx.serialize())
                Option(tx.getWitnessSet.getPlutusV1Scripts).foreach { scripts =>
                    v1Scripts ++= scripts.asScala.map(s => Utils.bytesToHex(s.getScriptHash))
                }
                Option(tx.getWitnessSet.getPlutusV2Scripts).foreach { scripts =>
                    v2Scripts ++= scripts.asScala.map(s => Utils.bytesToHex(s.getScriptHash))
                }
                if !tx.isValid then
                    println(
                      s"${Console.RED}AAAAA invalid!!! ${TransactionUtil.getTxHash(tx)} ${Console.RESET}"
                    )
                val result = evaluator.evaluateTx(tx, util.Set.of())
                totalTx += 1
                if !result.isSuccessful then
                    errors += result.getResponse
                    println(s"${Console.RED}AAAA!!!! ${TransactionUtil
                            .getTxHash(tx)} ${result.getResponse}${Console.RESET}")
//                println(tx.getWitnessSet.getRedeemers)
//                println("----------------------------------------------------")
//            println(s"=======================================")
        println(
          s"Total txs: $totalTx, errors: $errors, v1: ${v1Scripts.size}, v2: ${v2Scripts.size}"
        )

    }

    ignore("Blockfrost testnet evaluate tx with minting policy v2") {
        val apiKey = System.getenv("BLOCKFROST_API_KEY")
        val backendService = new BFBackendService(Constants.BLOCKFROST_MAINNET_URL, apiKey)
        val utxoSupplier = new DefaultUtxoSupplier(backendService.getUtxoService)
        val protocolParamsSupplier =
            new DefaultProtocolParamsSupplier(backendService.getEpochService)
        val evaluator =
            ScalusTransactionEvaluator(utxoSupplier, protocolParamsSupplier, scriptSupplier = null)
        val utxoSelector = new DefaultUtxoSelector(utxoSupplier)
        val utxoOptional = utxoSelector.findFirst(
          sender1Addr,
          utxo =>
              utxo.getAmount
                  .stream()
                  .anyMatch(a =>
                      CardanoConstants.LOVELACE.equals(a.getUnit) && a.getQuantity
                          .compareTo(ADAConversionUtil.adaToLovelace(2)) >= 0
                  )
        ); // Find an utxo with at least 2 ADA

        val utxo = utxoOptional.orElseThrow()
        val txId = ByteString.fromHex(utxo.getTxHash)
        val idx = BigInt(utxo.getOutputIndex)
        val validator =
            MintingPolicyV2.compiledMintingPolicyScriptV2.toUplc(generateErrorTraces = true)
        val evaledTokens =
            val tokensSIR = compile(AssocMap.singleton(hex"484f534b59", BigInt("1000000000000000")))
            tokensSIR.toUplc()

        val appliedValidator =
            validator $ txId $ idx $ evaledTokens
        val program = Program((1, 0, 0), appliedValidator)
        val script = PlutusV2Script.builder().cborHex(program.doubleCborHex).build()
        val scriptTx = new ScriptTx()
            .collectFrom(utxo)
            .mintAsset(
              script,
              new Asset("SCALUS", BigInteger.valueOf(1)),
              PlutusData.unit(),
              sender1Addr
            )
        val result = new QuickTxBuilder(backendService)
            .compose(scriptTx)
            .feePayer(sender1Addr)
            .withSigner(SignerProviders.signerFrom(sender1))
            .withTxEvaluator(evaluator)
            .withTxInspector(transaction => {
                System.out.println(transaction)
            })
            .completeAndWait()
    }
