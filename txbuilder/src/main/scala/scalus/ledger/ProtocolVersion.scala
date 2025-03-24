package scalus.ledger

import io.bullet.borer.{Decoder, Encoder, Reader, Writer}

/** Represents a Cardano protocol version */
case class ProtocolVersion(
    /** Major version (1-10) */
    major: Int,

    /** Minor version */
    minor: Int
):
    require(major >= 1 && major <= 10, s"Major version must be between 1 and 10, got $major")
    require(minor >= 0, s"Minor version must be non-negative, got $minor")

object ProtocolVersion:
    /** CBOR encoder for ProtocolVersion */
    given Encoder[ProtocolVersion] with
        def write(w: Writer, value: ProtocolVersion): Writer =
            w.writeArrayHeader(2)
            w.writeInt(value.major)
            w.writeInt(value.minor)
            w

    /** CBOR decoder for ProtocolVersion */
    given Decoder[ProtocolVersion] with
        def read(r: Reader): ProtocolVersion =
            val size = r.readArrayHeader()
            if size != 2 then
                r.validationFailure(s"Expected 2 elements for ProtocolVersion, got $size")

            val major = r.readInt()
            val minor = r.readInt()

            if major < 1 || major > 10 then
                r.validationFailure(s"Major version must be between 1 and 10, got $major")
            if minor < 0 then r.validationFailure(s"Minor version must be non-negative, got $minor")

            ProtocolVersion(major, minor)
