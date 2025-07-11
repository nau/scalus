package scalus.bloxbean

import com.bloxbean.cardano.client.api.util.CostModelUtil
import com.bloxbean.cardano.client.backend.api.DefaultUtxoSupplier
import com.bloxbean.cardano.client.backend.blockfrost.common.Constants
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import scalus.bloxbean.BlocksValidation.apiKey
import scalus.cardano.ledger.*
import scalus.cardano.ledger.Language.*
import scalus.ledger.api.MajorProtocolVersion
import scalus.uplc.eval.ExBudget

import java.nio.file.{Files, Path, Paths}

class BlockDeserializationTest extends AnyFunSuite {
    private val blocksDir = Paths.get(s"./blocks")

    test("decode block 11544748") {
        readBlockFromResources(11544748)
    }

    test("decode block 11544518") {
        readBlockFromResources(11544518)
    }

    test("decode block 11649988") {
        readBlockFromResources(11649988)
    }

    ignore("decode blocks of epoch 543") {
        val blocks = Files
            .list(blocksDir)
            .filter(f => f.getFileName.toString.endsWith(".cbor"))
            .sorted()
        blocks.forEach(readBlock)
    }

    test("evaluate block 11544748") {
        pending
        val bytes = getClass.getResourceAsStream(s"/blocks/block-11544748.cbor").readAllBytes()
        given OriginalCborByteArray = OriginalCborByteArray(bytes)
        val block = readBlock(bytes).block
        val txn = block.transactions.find(tx => tx.witnessSet.redeemers.nonEmpty).get
        validateTransaction(txn)
    }

    test("evaluate block 11544518") {
        pending
        val bytes = getClass.getResourceAsStream(s"/blocks/block-11544518.cbor").readAllBytes()
        given OriginalCborByteArray = OriginalCborByteArray(bytes)
        val block = readBlock(bytes).block
        val txn = block.transactions.find(tx => tx.witnessSet.redeemers.nonEmpty).get
        validateTransaction(txn)
    }

    private def validateTransaction(tx: Transaction): Unit = {
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
        val utxoResolver = ScalusUtxoResolver(utxoSupplier, scriptSupplier)
        val utxos = utxoResolver.resolveUtxos(tx)
        val costModels = CostModels(models =
            Map(
              PlutusV1.ordinal -> CostModelUtil.PlutusV1CostModel.getCosts.toIndexedSeq,
              PlutusV2.ordinal -> CostModelUtil.PlutusV2CostModel.getCosts.toIndexedSeq,
              PlutusV3.ordinal -> CostModelUtil.PlutusV3CostModel.getCosts.toIndexedSeq,
            )
        )
        val evaluator = PlutusScriptEvaluator(
          SlotConfig.Mainnet,
          initialBudget = ExBudget.fromCpuAndMemory(10_000000000L, 10_000000L),
          protocolMajorVersion = MajorProtocolVersion.plominPV,
          costModels = costModels,
          debugDumpFilesForTesting = false
        )
        //        dumpTxInfo(tx, utxos)

        val redeemers = evaluator.evalPhaseTwo(tx, utxos)
        for (actual, expected) <- redeemers.zip(tx.witnessSet.redeemers.get.value.toIndexedSeq) do
            assert(actual.exUnits.memory == expected.exUnits.memory)
            assert(actual.exUnits.steps == expected.exUnits.steps)
    }

    private def dumpTxInfo(
        tx: Transaction,
        utxos: Map[TransactionInput, TransactionOutput]
    ): Unit = {
        val txhash = tx.id.toHex
        Files.write(Paths.get(s"tx-$txhash.cbor"), Cbor.encode(tx).toByteArray)
        Files.deleteIfExists(Paths.get("scalus.log"))
        storeInsOutsInCborFiles(utxos, txhash)
    }

    private def storeInsOutsInCborFiles(
        utxos: Map[TransactionInput, TransactionOutput],
        txhash: String
    ): Unit = {
        val ins = Cbor.encode(utxos.keys.toIndexedSeq).toByteArray
        val outs = Cbor.encode(utxos.values.toIndexedSeq).toByteArray
        Files.write(Path.of(s"ins-$txhash.cbor"), ins)
        Files.write(Path.of(s"outs-$txhash.cbor"), outs)
    }

    private def readBlock(num: Int): BlockFile = readBlock(blocksDir.resolve(s"block-$num.cbor"))
    private def readBlock(path: Path): BlockFile = readBlock(Files.readAllBytes(path))
    private def readBlockFromResources(num: Int): BlockFile = readBlock(
      getClass.getResourceAsStream(s"/blocks/block-$num.cbor").readAllBytes()
    )
    private def readBlock(blockBytes: Array[Byte]): BlockFile = {
        BlockFile.fromCborArray(blockBytes)
    }
}
