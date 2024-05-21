package scalus.bloxbean
import com.bloxbean.cardano.client.backend.api.ScriptService
import com.bloxbean.cardano.client.plutus.spec.PlutusScript
import com.bloxbean.cardano.client.plutus.util.PlutusUtil
import scalus.utils.Utils

import java.nio.file.Files
import java.nio.file.Paths
import scala.collection.mutable

class CachedScriptSupplier(scriptService: ScriptService) extends ScriptSupplier {
    private val cache = mutable.Map[String, PlutusScript]()
    override def getScript(scriptHash: String): PlutusScript = {
        //                    println(s"getting script $scriptHash")
        if cache.contains(scriptHash) then cache(scriptHash)
        else if Files.exists(Paths.get(s"scripts/$scriptHash")) then
            //                println(s"found $scriptHash in scripts folder")
            val scriptBytes = Files.readAllBytes(Paths.get(s"scripts/$scriptHash"))
            val script =
                PlutusUtil.getPlutusScript(scriptHash, Utils.bytesToHex(scriptBytes)).get
            cache.put(scriptHash, script)
            script
        else
            val script = scriptService.getPlutusScript(scriptHash)
            if script.isSuccessful then
                val s = script.getValue
                cache.put(scriptHash, s)
                Files.write(Paths.get(s"scripts/$scriptHash"), Utils.hexToBytes(s.getCborHex))
                //                    println(s"queried $scriptHash in blockfrost and saved to scripts folder")
                s
            else throw new RuntimeException(s"Script not found $scriptHash")
    }
}
