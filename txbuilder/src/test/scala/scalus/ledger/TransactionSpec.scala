package scalus.ledger

import com.bloxbean.cardano.client.address.util.AddressUtil
import com.bloxbean.cardano.client.transaction.spec
import com.bloxbean.cardano.client.transaction.util.TransactionUtil.getTxHash
import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.{ByteString, JVMPlatformSpecific}
import scalus.utils.Utils

import java.math.BigInteger
import java.nio.file.{Files, Path, Paths}

class TransactionSpec extends AnyFunSuite {
    val addr = AddressUtil.addressToBytes(
      "addr1qxwg0u9fpl8dac9rkramkcgzerjsfdlqgkw0q8hy5vwk8tzk5pgcmdpe5jeh92guy4mke4zdmagv228nucldzxv95clqe35r3m"
    )
    test("TxnWitnessSet") {
        asdf()
        fdsa()
    }

    private def asdf(): Unit = {
        val tx = Transaction(
          TransactionBody(
            inputs = Set(
              TransactionInput(Hash32(ByteString.fill(32, 0)), 0)
            ),
            outputs = List(
              TransactionOutput.Babbage(
                address = Address(ByteString.fromArray(addr)),
                value = Value.lovelace(2)
              )
            ),
            fee = Coin(0)
          ),
          witnessSet = TransactionWitnessSet(),
          isValid = true
        )
//        println(tx)
        println(ByteString.fromArray(addr).toHex)
        println(Utils.bytesToHex(Cbor.encode(tx).toByteArray))
        val txbody = Cbor.encode(tx.body).toByteArray
        val txhash = JVMPlatformSpecific.blake2b_256(ByteString.fromArray(txbody))
        println(txhash)
    }

    private def fdsa(): Unit = {
        import com.bloxbean.cardano.client.transaction.spec.*
        val tx = Transaction
            .builder()
            .body(
              TransactionBody
                  .builder()
                  .inputs(
                    java.util.List.of(
                      TransactionInput
                          .builder()
                          .transactionId(ByteString.fill(32, 0).toHex)
                          .index(0)
                          .build()
                    )
                  )
                  .outputs(
                    java.util.List.of(
                      TransactionOutput
                          .builder()
                          .address(
                            "addr1qxwg0u9fpl8dac9rkramkcgzerjsfdlqgkw0q8hy5vwk8tzk5pgcmdpe5jeh92guy4mke4zdmagv228nucldzxv95clqe35r3m"
                          )
                          .value(Value.builder().coin(BigInteger.valueOf(2)).build())
                          .build()
                    )
                  )
                  .fee(BigInteger.valueOf(0))
                  .build()
            )
            .build()
//        println(tx)
        println(Utils.bytesToHex(tx.serialize()))
        val txhash = getTxHash(tx)
        println(txhash)
    }

    private val blocksDir = Paths.get(s"../bloxbean-cardano-client-lib/blocks")

    test("decode blocks of epoch 543") {
        val blocks = Files
            .list(blocksDir)
            .filter(f => f.getFileName.toString.endsWith(".cbor"))
            .sorted()
        blocks.forEach(readBlock)
    }

    test("decode block") {
        readBlock(11649988)
    }

    private def readBlock(num: Int): Unit = {
        readBlock(blocksDir.resolve(s"block-$num.cbor"))
    }

    private def readBlock(path: Path): Unit = {
        val blockBytes = Files.readAllBytes(path)
        try
            Cbor.decode(blockBytes).to[BlockFile].value
//            println(s"Decoded block $path")
        catch
            case e: Exception =>
                println(s"Error reading block $path: ${e.getMessage}")
                e.printStackTrace()
                fail()
        //                val dom = Cbor.decode(blockBytes).to[Dom.Element].value
        //                println(dom.render())
    }

}
