package scalus.bloxbean
import com.bloxbean.cardano.yaci.core.config.YaciConfig
import com.bloxbean.cardano.yaci.core.model.*
import com.bloxbean.cardano.yaci.core.protocol.chainsync.messages.Point
import com.bloxbean.cardano.yaci.helper.BlockRangeSync
import com.bloxbean.cardano.yaci.helper.listener.BlockChainDataListener
import com.bloxbean.cardano.yaci.helper.model.Transaction

import java.nio.file.Files
import java.nio.file.Path
import java.util

object TxDownloader {

    @main
    def main(): Unit = {
        YaciConfig.INSTANCE.setReturnBlockCbor(true)
        val blockRangeSync =
            new BlockRangeSync("178.18.251.55", 6000, 764824073L);
        blockRangeSync.start(new BlockChainDataListener() {
            override def onBlock(
                era: Era,
                block: Block,
                transactions: util.List[Transaction]
            ): Unit = {
                println(s"Block ${block.getHeader.getHeaderBody.getBlockNumber}")
                Files.write(
                  Path.of(
                    s"/Users/nau/projects/scalus/bloxbean-cardano-client-lib/blocks/block-${block.getHeader.getHeaderBody.getBlockNumber}.cbor"
                  ),
                  util.List.of(block.getCbor)
                )
            }
        })
        // full epoch 484,
        val from =
            Point(134092810, "bd14aacfeb71fb52313b58cc0707634eee04ac71e61c438de83ea8ee8b7bd31a")
        val to =
            Point(134524753, "29011cc1320d03b3da0121236dc66e6bc391feef4bb1d506a7fb20e769d6a494")
        blockRangeSync.fetch(from, to)
    }

}
