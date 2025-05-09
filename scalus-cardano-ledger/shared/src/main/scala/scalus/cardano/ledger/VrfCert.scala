package scalus.cardano.ledger

import io.bullet.borer.{Decoder, Encoder, Reader, Writer}
import scalus.builtin.ByteString

/** Represents a VRF certificate in Cardano, consisting of output and proof */
case class VrfCert(
    /** VRF output */
    output: ByteString,

    /** VRF proof (80 bytes) */
    proof: ByteString
):
    require(proof.size == 80, s"VRF proof must be 80 bytes, got ${proof.size}")

object VrfCert:
    /** CBOR encoder for VrfCert */
    given Encoder[VrfCert] with
        def write(w: Writer, value: VrfCert): Writer =
            w.writeArrayHeader(2)
            w.writeBytes(value.output.bytes)
            w.writeBytes(value.proof.bytes)
            w

    /** CBOR decoder for VrfCert */
    given Decoder[VrfCert] with
        def read(r: Reader): VrfCert =
            val size = r.readArrayHeader()
            if size != 2 then r.validationFailure(s"Expected 2 elements for VrfCert, got $size")

            val output = ByteString.unsafeFromArray(r.readBytes())
            val proof = ByteString.unsafeFromArray(r.readBytes())

            if proof.size != 80 then
                r.validationFailure(s"VRF proof must be 80 bytes, got ${proof.size}")

            VrfCert(output, proof)
