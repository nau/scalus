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
}

object BlocksLedgerRulesValidator {
    private val utxoResolver = ResourcesUtxoResolver()
}
