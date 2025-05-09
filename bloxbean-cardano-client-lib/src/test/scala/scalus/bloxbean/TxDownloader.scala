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
            Point(149644779, "ac4ddd1cc64d30e94a42fc735dae4faa4556db1cbc5ea3caeaafc7916c4e7a4e")
        val to =
            Point(151605230, "7129f03cee5f225091d0ba1b47385c8b1a2ac3f0152e20bfa9c96c798051521f")
        blockRangeSync.fetch(from, to)
    }

}
