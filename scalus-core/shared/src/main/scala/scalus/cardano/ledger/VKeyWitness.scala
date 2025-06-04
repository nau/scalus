package scalus.cardano.ledger

import io.bullet.borer.{Codec, Decoder, Encoder, Reader, Writer}
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import scalus.builtin.ByteString

/** Represents a verification key witness in Cardano */
case class VKeyWitness(
    /** Verification key (32 bytes) */
    vkey: ByteString,

    /** Signature (64 bytes) */
    signature: ByteString
) derives Codec:
    require(vkey.size == 32, s"Verification key must be 32 bytes, got ${vkey.size}")
    require(signature.size == 64, s"Signature must be 64 bytes, got ${signature.size}")
