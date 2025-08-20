package scalus.bloxbean
import com.bloxbean.cardano.client.backend.api.ScriptService
import com.bloxbean.cardano.client.plutus.spec.PlutusScript
import com.bloxbean.cardano.client.plutus.util.PlutusUtil
import scalus.utils.Utils

import java.nio.file.Files
import java.util
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import java.nio.file.Path

trait ScriptSupplier {
    def getScript(scriptHash: String): PlutusScript
}

class NoScriptSupplier extends ScriptSupplier {
    override def getScript(scriptHash: String): PlutusScript = throw new RuntimeException(
      s"No additional scripts allowed but script hash requested: $scriptHash"
    )
}

class MapScriptSupplier(scripts: util.Map[String, PlutusScript]) extends ScriptSupplier {
    override def getScript(scriptHash: String): PlutusScript = scripts.asScala.getOrElse(
      scriptHash,
      throw new RuntimeException(s"Script not found $scriptHash")
    )
}

class ScriptServiceSupplier(scriptService: ScriptService) extends ScriptSupplier {
    override def getScript(scriptHash: String): PlutusScript = {
        val script = scriptService.getPlutusScript(scriptHash)
        if script.isSuccessful then script.getValue
        else throw new RuntimeException(s"Script not found $scriptHash")
    }
}

class InMemoryCachedScriptSupplier(
    scriptSupplier: ScriptSupplier,
    initialCache: util.Map[String, PlutusScript] = util.Map.of()
) extends ScriptSupplier {
    private val cache = mutable.HashMap.from(initialCache.asScala)
    override def getScript(scriptHash: String): PlutusScript =
        cache.getOrElseUpdate(
          scriptHash,
          scriptSupplier.getScript(scriptHash)
        )
}

class FileScriptSupplier(scriptsDirectory: Path, scriptSupplier: ScriptSupplier)
    extends ScriptSupplier {
    override def getScript(scriptHash: String): PlutusScript =
        if Files.exists(scriptsDirectory.resolve(scriptHash)) then
            val scriptBytes = Files.readAllBytes(scriptsDirectory.resolve(scriptHash))
            val script = PlutusUtil.getPlutusScript(scriptHash, Utils.bytesToHex(scriptBytes)).get
            script
        else
            val script = scriptSupplier.getScript(scriptHash)
            Files.createDirectories(scriptsDirectory)
            Files.write(scriptsDirectory.resolve(scriptHash), Utils.hexToBytes(script.getCborHex))
            script
}
