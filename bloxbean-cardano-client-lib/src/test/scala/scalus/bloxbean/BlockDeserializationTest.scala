package scalus.bloxbean

import com.bloxbean.cardano.client.api.util.CostModelUtil
import com.bloxbean.cardano.client.backend.api.DefaultUtxoSupplier
import com.bloxbean.cardano.client.backend.blockfrost.common.Constants
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import org.scalatest.funsuite.AnyFunSuite
import scalus.bloxbean.BlocksValidation.apiKey
import scalus.cardano.ledger.*
import scalus.ledger.api.MajorProtocolVersion
import scalus.uplc.eval.ExBudget

import java.nio.file.{Files, Path, Paths}

class BlockDeserializationTest extends AnyFunSuite {
    private val blocksDir = Paths.get(s"./blocks")

    test("evaluate block 11544748") {
        pending
        val bytes = getClass.getResourceAsStream(s"/blocks/block-11544748.cbor").readAllBytes()
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
        val costMdls = com.bloxbean.cardano.client.plutus.spec.CostMdls()
        costMdls.add(CostModelUtil.PlutusV1CostModel)
        costMdls.add(CostModelUtil.PlutusV2CostModel)
        val evaluator = NewTxEvaluator(
          SlotConfig.Mainnet,
          initialBudget = ExBudget.fromCpuAndMemory(10_000000000L, 10_000000L),
          protocolMajorVersion = MajorProtocolVersion.plominPV,
          costMdls = costMdls
        )
        evaluator.evalPhaseTwo(tx, utxos)
    }

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

    private def readBlock(num: Int): BlockFile = readBlock(blocksDir.resolve(s"block-$num.cbor"))
    private def readBlock(path: Path): BlockFile = readBlock(Files.readAllBytes(path))
    private def readBlockFromResources(num: Int): BlockFile = readBlock(
      getClass.getResourceAsStream(s"/blocks/block-$num.cbor").readAllBytes()
    )
    private def readBlock(blockBytes: Array[Byte]): BlockFile = {
        BlockFile.fromCborArray(blockBytes)
    }
}
