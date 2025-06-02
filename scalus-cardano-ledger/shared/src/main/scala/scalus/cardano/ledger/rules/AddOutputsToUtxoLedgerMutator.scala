package scalus.cardano.ledger.rules

import scalus.builtin.{ByteString, PlatformSpecific, given}
import scalus.cardano.ledger.{Hash32, TransactionInput}
import io.bullet.borer.Cbor

object AddOutputsToUtxoLedgerMutator extends Ledger.STS.Mutator[Ledger.StateI.Utxo] {
    override def transit[StateT: StateI](
        context: Context,
        state: StateT,
        event: Event
    ): Either[Error, StateT] = {
        val transactionId = Hash32(
          summon[PlatformSpecific].blake2b_256(
            ByteString.unsafeFromArray(Cbor.encode(event.body).toByteArray)
          )
        )

        val addedUtxo: Utxo = event.body.outputs.view.zipWithIndex.map { case (output, index) =>
            TransactionInput(transactionId, index) -> output
        }.toMap

        Right(state.utxo = state.utxo ++ addedUtxo)
    }
}
