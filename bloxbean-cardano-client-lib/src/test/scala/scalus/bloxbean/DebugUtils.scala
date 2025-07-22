package scalus.bloxbean

import io.bullet.borer.Cbor
import scalus.cardano.ledger.{Transaction, TransactionInput, TransactionOutput}

import java.nio.file.{Files, Path, Paths}

private[scalus] object DebugUtils {
    def dumpTxInfo(
        tx: Transaction,
        utxos: Map[TransactionInput, TransactionOutput]
    ): Unit = {
        val txhash = tx.id.toHex
        Files.write(Paths.get(s"tx-$txhash.cbor"), tx.toCbor)
        Files.deleteIfExists(Paths.get("scalus.log"))
        storeInsOutsInCborFiles(utxos, txhash)
    }

    def storeInsOutsInCborFiles(
        utxos: Map[TransactionInput, TransactionOutput],
        txhash: String
    ): Unit = {
        val ins = Cbor.encode(utxos.keys.toIndexedSeq).toByteArray
        val outs = Cbor.encode(utxos.values.toIndexedSeq).toByteArray
        Files.write(Path.of(s"ins-$txhash.cbor"), ins)
        Files.write(Path.of(s"outs-$txhash.cbor"), outs)
    }
}
