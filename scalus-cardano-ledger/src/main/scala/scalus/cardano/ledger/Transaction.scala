package scalus.cardano.ledger

import io.bullet.borer.NullOptions.given
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.*

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
) derives Codec
