package scalus.bloxbean
import com.bloxbean.cardano.client.api.UtxoSupplier
import com.bloxbean.cardano.client.api.common.OrderEnum
import com.bloxbean.cardano.client.api.model.Utxo
import com.fasterxml.jackson.databind.ObjectMapper

import java.io.File
import java.nio.file.Files
import java.nio.file.Paths
import java.util
import java.util.Optional
import scala.collection.mutable

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
