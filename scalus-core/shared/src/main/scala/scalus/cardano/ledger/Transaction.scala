package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.NullOptions.given
import io.bullet.borer.derivation.ArrayBasedCodecs.*

/** Represents a complete transaction in Cardano */
case class Transaction(
    /** The transaction body */
    body: KeepRaw[TransactionBody],

    /** Witness set containing signatures and scripts */
    witnessSet: TransactionWitnessSet,

    /** Is the transaction valid? */
    isValid: Boolean,

    /** Optional auxiliary data */
    auxiliaryData: Option[AuxiliaryData] = None
)

object Transaction {

    /** CBOR codec for Transaction */
    given Encoder[Transaction] = Encoder.derived
    given decoder(using OriginalCborByteArray): Decoder[Transaction] =
        Decoder.derived[Transaction]
}
