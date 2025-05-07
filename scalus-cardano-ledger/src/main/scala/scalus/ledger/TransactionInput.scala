package scalus.ledger

import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.*

/** Represents an input to a transaction
  *
  * A transaction input is a reference to an output from a previous transaction, identified by the
  * transaction ID and the index of the output.
  */
final case class TransactionInput(
    /** The ID of the transaction that produced the output we're spending */
    transactionId: Hash32,

    /** The index of the output in the transaction we're spending */
    index: Int
) derives Codec
