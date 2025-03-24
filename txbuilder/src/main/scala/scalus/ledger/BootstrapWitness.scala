package scalus.ledger

import scalus.builtin.ByteString
import io.bullet.borer.{Decoder, Encoder, Reader, Writer}

/** Represents a bootstrap witness in Cardano (for Byron-era addresses) */
case class BootstrapWitness(
    /** Public key (32 bytes) */
    publicKey: ByteString,

    /** Signature (64 bytes) */
    signature: ByteString,

    /** Chain code (32 bytes) */
    chainCode: ByteString,

    /** Attributes */
    attributes: ByteString
):
    require(publicKey.size == 32, s"Public key must be 32 bytes, got ${publicKey.size}")
    require(signature.size == 64, s"Signature must be 64 bytes, got ${signature.size}")
    require(chainCode.size == 32, s"Chain code must be 32 bytes, got ${chainCode.size}")

object BootstrapWitness:
    /** CBOR encoder for BootstrapWitness */
    given Encoder[BootstrapWitness] with
        def write(w: Writer, value: BootstrapWitness): Writer =
            w.writeArrayHeader(4)
            w.writeBytes(value.publicKey.bytes)
            w.writeBytes(value.signature.bytes)
            w.writeBytes(value.chainCode.bytes)
            w.writeBytes(value.attributes.bytes)
            w

    /** CBOR decoder for BootstrapWitness */
    given Decoder[BootstrapWitness] with
        def read(r: Reader): BootstrapWitness =
            val size = r.readArrayHeader()
            if size != 4 then
                r.validationFailure(s"Expected 4 elements for BootstrapWitness, got $size")

            val publicKey = ByteString.unsafeFromArray(r.readBytes())
            val signature = ByteString.unsafeFromArray(r.readBytes())
            val chainCode = ByteString.unsafeFromArray(r.readBytes())
            val attributes = ByteString.unsafeFromArray(r.readBytes())

            if publicKey.size != 32 then
                r.validationFailure(s"Public key must be 32 bytes, got ${publicKey.size}")
            if signature.size != 64 then
                r.validationFailure(s"Signature must be 64 bytes, got ${signature.size}")
            if chainCode.size != 32 then
                r.validationFailure(s"Chain code must be 32 bytes, got ${chainCode.size}")

            BootstrapWitness(publicKey, signature, chainCode, attributes)
