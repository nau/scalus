package scalus.bloxbean

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*

import java.nio.file.{Files, Path, Paths}

class BlockDeserializationTest extends AnyFunSuite {
    private val blocksDir = Paths.get(s"./blocks")

    test("evaluate block 11544748") {
        val bytes = getClass.getResourceAsStream(s"/blocks/block-11544748.cbor").readAllBytes()
        given OriginalCborByteArray = OriginalCborByteArray(bytes)
        val block = readBlock(bytes).block
        val txn = block.transactions.find(tx => tx.witnessSet.redeemers.nonEmpty).get
        validateTransaction(txn)

    }

    private def validateTransaction(tx: Transaction): Unit = {
        println(tx.witnessSet.redeemers)
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
