package scalus.ledger

import scalus.builtin.ByteString
import io.bullet.borer.{Decoder, Encoder, Reader, Writer}

/** Represents an operational certificate in Cardano
  *
  * An operational certificate authorizes a node to create blocks
  */
case class OperationalCert(
    /** Hot key verification key (KES) */
    hotVKey: ByteString,

    /** Sequence number */
    sequenceNumber: Long,

    /** KES period */
    kesPeriod: Long,

    /** Sigma (signature) */
    sigma: ByteString
):
    require(hotVKey.size == 32, s"Hot key must be 32 bytes, got ${hotVKey.size}")
    require(sequenceNumber >= 0, s"Sequence number must be non-negative, got $sequenceNumber")
    require(kesPeriod >= 0, s"KES period must be non-negative, got $kesPeriod")
    require(sigma.size == 64, s"Signature must be 64 bytes, got ${sigma.size}")

object OperationalCert:
    /** CBOR encoder for OperationalCert */
    given Encoder[OperationalCert] with
        def write(w: Writer, value: OperationalCert): Writer =
            w.writeArrayHeader(4)
            w.writeBytes(value.hotVKey.bytes)
            w.writeLong(value.sequenceNumber)
            w.writeLong(value.kesPeriod)
            w.writeBytes(value.sigma.bytes)
            w

    /** CBOR decoder for OperationalCert */
    given Decoder[OperationalCert] with
        def read(r: Reader): OperationalCert =
            val size = r.readArrayHeader()
            if size != 4 then
                r.validationFailure(s"Expected 4 elements for OperationalCert, got $size")

            val hotVKey = ByteString.unsafeFromArray(r.readBytes())
            val sequenceNumber = r.readLong()
            val kesPeriod = r.readLong()
            val sigma = ByteString.unsafeFromArray(r.readBytes())

            if hotVKey.size != 32 then
                r.validationFailure(s"Hot key must be 32 bytes, got ${hotVKey.size}")
            if sequenceNumber < 0 then
                r.validationFailure(s"Sequence number must be non-negative, got $sequenceNumber")
            if kesPeriod < 0 then
                r.validationFailure(s"KES period must be non-negative, got $kesPeriod")
            if sigma.size != 64 then
                r.validationFailure(s"Signature must be 64 bytes, got ${sigma.size}")

            OperationalCert(hotVKey, sequenceNumber, kesPeriod, sigma)
