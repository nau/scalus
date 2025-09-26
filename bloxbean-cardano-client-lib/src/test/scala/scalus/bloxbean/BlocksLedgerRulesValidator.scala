package scalus.bloxbean

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import org.scalatest.funsuite.AnyFunSuite

class BlocksLedgerRulesValidator(
    val sts: STS,
    val context: Context = Context(),
    val state: State = State()
) extends AnyFunSuite {
    import BlocksLedgerRulesValidator.utxoResolver

    test("check commited blocks") {
        val url = getClass.getResource("/blocks/")
        val dirPath = Paths.get(url.toURI)

        val files = Files.list(dirPath).iterator().asScala.toList
        for file <- files
        do
            val bytes = Files.readAllBytes(file)
            given OriginalCborByteArray = OriginalCborByteArray(bytes)
            val block = BlockFile.fromCborArray(bytes).block
            for transaction <- block.transactions
            do
                try
                    val utxo = utxoResolver.resolveUtxos(transaction)
//                    val utxo = bloxbeanResolveUtxo(transaction)

                    assert(sts(context, state.copy(utxo = state.utxo ++ utxo), transaction).isRight)

//                    println(
//                      s"Transaction ${transaction.id} in block ${block.header.blockNumber} passed ${sts.getClass.getSimpleName}"
//                    )

                catch
                    case e: IllegalStateException
                        if e.getMessage.startsWith("UTXO not found for input") =>

//                        println(
//                          s"Skipping transaction ${transaction.id} in block ${block.header.blockNumber} due to missing UTXO"
//                        )
    }

    private def bloxbeanResolveUtxo(tx: Transaction): Map[TransactionInput, TransactionOutput] = {
        import com.bloxbean.cardano.client.backend.api.DefaultUtxoSupplier
        import com.bloxbean.cardano.client.backend.blockfrost.common.Constants
        import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService

        // this makes sure to download the utxos in the same directory that `resolveUtxoFromResources` is going to
        // look for them in.
        val blocksUrl = getClass.getResource("/blocks")
        val compiledResourcesPath = Paths.get(blocksUrl.toURI).getParent
        val resourcesPath = compiledResourcesPath.getParent.getParent.getParent
            .resolve("src")
            .resolve("test")
            .resolve("resources")

        val backendService =
            new BFBackendService(Constants.BLOCKFROST_MAINNET_URL, BlocksValidation.apiKey)
        val utxoSupplier = CachedUtxoSupplier(
          resourcesPath.resolve("utxos"),
          DefaultUtxoSupplier(backendService.getUtxoService)
        )
        // memory and file cached script supplier using the script service
        val scriptSupplier = InMemoryCachedScriptSupplier(
          FileScriptSupplier(
            resourcesPath.resolve("scripts"),
            ScriptServiceSupplier(backendService.getScriptService)
          )
        )
        val utxoResolver = ScalusUtxoResolver(utxoSupplier, scriptSupplier)
        utxoResolver.resolveUtxos(tx)
    }
}

object BlocksLedgerRulesValidator {
    private val utxoResolver = ResourcesUtxoResolver()
}
