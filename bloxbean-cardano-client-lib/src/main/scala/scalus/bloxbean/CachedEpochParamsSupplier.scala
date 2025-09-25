package scalus.bloxbean

import com.bloxbean.cardano.client.api.UtxoSupplier
import com.bloxbean.cardano.client.api.common.OrderEnum
import com.bloxbean.cardano.client.api.model.{ProtocolParams, Result, Utxo}
import com.fasterxml.jackson.databind.ObjectMapper

import java.nio.file.{Files, Path}
import java.util
import java.util.Optional
import scala.collection.mutable
import com.bloxbean.cardano.client.backend.api.EpochService
import com.bloxbean.cardano.client.backend.model.EpochContent

import scala.reflect.ClassTag

class CachedEpochParamsSupplier(cachePath: Path, default: EpochService) extends EpochService {
    private val objectMapper = new ObjectMapper()

    private val cacheEpochContent = mutable.HashMap[Integer, EpochContent]()
    private val cacheProtocolParams = mutable.HashMap[Integer, ProtocolParams]()

    private def success[T](value: T): Result[T] =
        Result
            .success("")
            .asInstanceOf[Result[T]]
            .withValue(value)
            .asInstanceOf[Result[T]]

    private def cache[K, V](id: K)(
        classOf: Class[V],
        prefix:String,
        cached: K => Option[V],
        store: (K, V) => Unit,
        read: K => Result[V]
    ): Result[V] = cached(id).fold {
        val file = cachePath.resolve(s"$prefix-$id").toFile()
        if file.exists() then {
            val value = objectMapper.readValue(file, classOf)
            store(id, value)
            success(value)
        } else {
            val value = read(id)
            if value.isSuccessful then
                store(id, value.getValue)
                // Ensure parent directory exists before writing
                Files.createDirectories(file.getParentFile.toPath)
                objectMapper.writeValue(file, value.getValue)
            value
        }
    }(success)

    override def getLatestEpoch: Result[EpochContent] = default.getLatestEpoch
    override def getProtocolParameters: Result[ProtocolParams] = default.getProtocolParameters

    override def getEpoch(epoch: Integer): Result[EpochContent] = cache(epoch)(
      classOf[EpochContent],
      "epochContent",
      cacheEpochContent.get,
      cacheEpochContent.put,
      default.getEpoch
    )

    override def getProtocolParameters(epoch: Integer): Result[ProtocolParams] = cache(epoch)(
      classOf[ProtocolParams],
      "protocolParams",
      cacheProtocolParams.get,
      cacheProtocolParams.put,
      default.getProtocolParameters
    )

}
