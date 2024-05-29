package scalus.bloxbean
import com.bloxbean.cardano.client.api.UtxoSupplier
import com.bloxbean.cardano.client.api.common.OrderEnum
import com.bloxbean.cardano.client.api.model.Utxo
import com.fasterxml.jackson.databind.ObjectMapper

import java.nio.file.Path
import java.util
import java.util.Optional
import scala.collection.mutable

class CachedUtxoSupplier(cachePath: Path, default: UtxoSupplier) extends UtxoSupplier {
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
        else
            val file = cachePath.resolve(s"$txHash-$outputIndex").toFile()
            if file.exists() then
                //                println(s"found $txHash-$outputIndex in utxos folder")
                val utxo = objectMapper.readValue(file, classOf[Utxo])
                cache.put((txHash, outputIndex), utxo)
                Optional.of(utxo)
            else
                val utxo = default.getTxOutput(txHash, outputIndex)
                utxo.ifPresent({ u =>
                    cache.put((txHash, outputIndex), u)
                    objectMapper.writeValue(file, u)
                    //                    println(s"queried $txHash-$outputIndex in blockfrost and saved to utxos folder")
                })
                utxo
    }
}
