package scalus.cardano.ledger
package rules

import io.bullet.borer.Cbor
import scalus.builtin.{ByteString, PlatformSpecific, given}
import scalus.cardano.ledger.{Hash, TransactionInput}

// It's part of Shelley.updateUTxOState in cardano-ledger
object AddOutputsToUtxoMutator extends STS.Mutator {
    override def transit(context: Context, state: State, event: Event): Result = {
        val transactionId: TransactionHash = Hash(
          summon[PlatformSpecific].blake2b_256(
            ByteString.unsafeFromArray(Cbor.encode(event.body).toByteArray)
          )
        )

        val addedUtxo: Utxo = event.body.value.outputs.view.zipWithIndex.map { case (output, index) =>
            TransactionInput(transactionId, index) -> output
        }.toMap

        success(state.copy(utxo = state.utxo ++ addedUtxo))
    }
}
