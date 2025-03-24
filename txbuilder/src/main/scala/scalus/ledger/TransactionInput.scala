package scalus.ledger

import scalus.builtin.ByteString
import io.bullet.borer.{Decoder, Encoder, Reader, Writer}

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
)

object TransactionInput {

    /** CBOR encoder for TransactionInput */
    given Encoder[TransactionInput] = Encoder { (w, input) =>
        w.writeArrayHeader(2)
            .write(input.transactionId)
            .writeInt(input.index)
    }

    /** CBOR decoder for TransactionInput */
    given Decoder[TransactionInput] = Decoder { r =>
        r.readArrayHeader()
        val txId = r.read[Hash32]()
        val index = r.readInt()
        TransactionInput(txId, index)
    }

    /** CBOR encoder for a set (list) of TransactionInput */
    given Encoder[Set[TransactionInput]] = Encoder { (w, inputs) =>
        w.writeArrayHeader(inputs.size)
        inputs.foreach(input => w.write(input))
        w
    }

//    /** CBOR decoder for a set (list) of TransactionInput */
//    given Decoder[Set[TransactionInput]] = Decoder { r =>
//        val count = r.readArrayHeader()
//        val inputs = for (_ <- 0L until count) yield r.read[TransactionInput]
//        inputs.toSet
//    }
}
