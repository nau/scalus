package scalus.bloxbean
import com.bloxbean.cardano.yaci.core.config.YaciConfig
import com.bloxbean.cardano.yaci.core.model.*
import com.bloxbean.cardano.yaci.core.protocol.chainsync.messages.Point
import com.bloxbean.cardano.yaci.helper.BlockRangeSync
import com.bloxbean.cardano.yaci.helper.listener.BlockChainDataListener
import com.bloxbean.cardano.yaci.helper.model.Transaction
import scalus.utils.Utils

import java.nio.file.{Files, Path, StandardOpenOption}
import java.util

object TxDownloader {

    @main
    def main(args: String*): Unit = {
        YaciConfig.INSTANCE.setReturnBlockCbor(true)
        val host = args.headOption.getOrElse("localhost")
        val blockRangeSync = new BlockRangeSync(host, 3001, 764824073L);
        blockRangeSync.start(new BlockChainDataListener() {
            override def onBlock(
                era: Era,
                block: Block,
                transactions: util.List[Transaction]
            ): Unit = {
                println(s"Block ${block.getHeader.getHeaderBody.getBlockNumber}")
                try
                    val cborBytes = Utils.hexToBytes(block.getCbor)
                    Files.createDirectories(Path.of("blocks"))
                    Files.write(
                      Path.of(
                        s"blocks/block-${block.getHeader.getHeaderBody.getBlockNumber}.cbor"
                      ),
                      cborBytes,
                      StandardOpenOption.TRUNCATE_EXISTING,
                      StandardOpenOption.WRITE,
                      StandardOpenOption.CREATE
                    )
                catch
                    case e: Exception =>
                        e.printStackTrace()
            }
        })
        val from =
            Point(151372807, "7325c3a11f265e60e78ce8fbb08cc109d3cf432d47c8d2d5dd432803d56a9147")
        val to =
            Point(151516583, "7462f5d03a4a4a0e11f77964b66a630184d39e437acb5a3104e66b4aa73907de")
        blockRangeSync.fetch(from, to)
    }

}
