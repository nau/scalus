package scalus.ledger

import io.bullet.borer.{Decoder, Encoder, Nullable, Reader, Writer}

/** Represents a complete transaction in Cardano */
case class Transaction(
    /** The transaction body */
    body: TransactionBody,

    /** Witness set containing signatures and scripts */
    witnessSet: TransactionWitnessSet,

    /** Is the transaction valid? */
    isValid: Boolean,

    /** Optional auxiliary data */
    auxiliaryData: Option[AuxiliaryData] = None
)

object Transaction:
    /** CBOR encoder for Transaction */
    given Encoder[Transaction] with
        def write(w: Writer, value: Transaction): Writer =
            w.writeArrayHeader(4)
            // Transaction body
            w.write(value.body)
            // Witness set
            w.write(value.witnessSet)
            // Is valid flag
            w.writeBoolean(value.isValid)
            // Auxiliary data (or nil)
            w.write[Nullable[Option[AuxiliaryData]]](value.auxiliaryData)
            w

    /** CBOR decoder for Transaction */
    given Decoder[Transaction] with
        def read(r: Reader): Transaction =
            val size = r.readArrayHeader()
            if size != 4 then r.validationFailure(s"Expected 4 elements for Transaction, got $size")

            // Transaction body
            val body = r.read[TransactionBody]()
            // Witness set
            val witnessSet = r.read[TransactionWitnessSet]()
            // Is valid flag
            val isValid = r.readBoolean()
            // Auxiliary data (or nil)
            val auxiliaryData = r.read[Nullable[Option[AuxiliaryData]]]()

            Transaction(body, witnessSet, isValid, auxiliaryData)
