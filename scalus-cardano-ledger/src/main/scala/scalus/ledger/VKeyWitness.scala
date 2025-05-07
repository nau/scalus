package scalus.ledger

import scalus.builtin.ByteString
import io.bullet.borer.{Decoder, Encoder, Reader, Writer}

/** Represents a verification key witness in Cardano */
case class VKeyWitness(
    /** Verification key (32 bytes) */
    vkey: ByteString,

    /** Signature (64 bytes) */
    signature: ByteString
):
    require(vkey.size == 32, s"Verification key must be 32 bytes, got ${vkey.size}")
    require(signature.size == 64, s"Signature must be 64 bytes, got ${signature.size}")

object VKeyWitness:
    /** CBOR encoder for VKeyWitness */
    given Encoder[VKeyWitness] with
        def write(w: Writer, value: VKeyWitness): Writer =
            w.writeArrayHeader(2)
            w.writeBytes(value.vkey.bytes)
            w.writeBytes(value.signature.bytes)
            w

    /** CBOR decoder for VKeyWitness */
    given Decoder[VKeyWitness] with
        def read(r: Reader): VKeyWitness =
            val size = r.readArrayHeader()
            if size != 2 then r.validationFailure(s"Expected 2 elements for VKeyWitness, got $size")

            val vkey = ByteString.unsafeFromArray(r.readBytes())
            val signature = ByteString.unsafeFromArray(r.readBytes())

            if vkey.size != 32 then
                r.validationFailure(s"Verification key must be 32 bytes, got ${vkey.size}")
            if signature.size != 64 then
                r.validationFailure(s"Signature must be 64 bytes, got ${signature.size}")

            VKeyWitness(vkey, signature)
