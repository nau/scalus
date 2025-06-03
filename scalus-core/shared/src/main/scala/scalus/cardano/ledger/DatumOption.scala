package scalus.cardano.ledger

import io.bullet.borer.Codec
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.derivation.key
import scalus.builtin.Data
import scalus.cardano.ledger.Hash.DataHash

/** Represents a datum option in Cardano outputs */
enum DatumOption derives Codec.All:
    /** Reference to a datum by its hash */
    @key(0) case Hash(hash: DataHash)

    /** Inline datum value */
    @key(1) case Inline(data: Data)
