package scalus.ledger

import scalus.builtin.ByteString
import io.bullet.borer.{Decoder, Encoder, Reader, Writer}

/** Represents the body of a block header in Cardano */
case class HeaderBody(
    /** Block number */
    blockNumber: Long,

    /** Slot number */
    slot: Long,

    /** Previous block hash (or nil for genesis) */
    prevHash: Option[Hash32],

    /** Issuer verification key */
    issuerVkey: ByteString,

    /** VRF verification key */
    vrfVkey: ByteString,

    /** VRF certificate */
    vrfResult: VrfCert,

    /** Block body size in bytes */
    blockBodySize: Long,

    /** Block body hash */
    blockBodyHash: Hash32,

    /** Operational certificate */
    operationalCert: OperationalCert,

    /** Protocol version */
    protocolVersion: ProtocolVersion
):
    require(blockNumber >= 0, s"Block number must be non-negative, got $blockNumber")
    require(slot >= 0, s"Slot must be non-negative, got $slot")
    require(
      issuerVkey.size == 32,
      s"Issuer verification key must be 32 bytes, got ${issuerVkey.size}"
    )
    require(vrfVkey.size == 32, s"VRF verification key must be 32 bytes, got ${vrfVkey.size}")
    require(blockBodySize >= 0, s"Block body size must be non-negative, got $blockBodySize")

object HeaderBody:
    /** CBOR encoder for HeaderBody */
    given Encoder[HeaderBody] with
        def write(w: Writer, value: HeaderBody): Writer =
            w.writeArrayHeader(10)

            // Block number
            w.writeLong(value.blockNumber)

            // Slot
            w.writeLong(value.slot)

            // Previous hash (or nil)
            value.prevHash match
                case Some(hash) => Hash32.given_Encoder_Hash32.write(w, hash)
                case None       => w.writeNull()

            // Issuer verification key
            w.writeBytes(value.issuerVkey.bytes)

            // VRF verification key
            w.writeBytes(value.vrfVkey.bytes)

            // VRF result
            VrfCert.given_Encoder_VrfCert.write(w, value.vrfResult)

            // Block body size
            w.writeLong(value.blockBodySize)

            // Block body hash
            Hash32.given_Encoder_Hash32.write(w, value.blockBodyHash)

            // Operational certificate
            OperationalCert.given_Encoder_OperationalCert.write(w, value.operationalCert)

            // Protocol version
            ProtocolVersion.given_Encoder_ProtocolVersion.write(w, value.protocolVersion)

            w

    /** CBOR decoder for HeaderBody */
    given Decoder[HeaderBody] with
        def read(r: Reader): HeaderBody =
            val size = r.readArrayHeader()
            if size != 10 then
                r.validationFailure(s"Expected 10 elements for HeaderBody, got $size")

            val blockNumber = r.readLong()
            val slot = r.readLong()

            // Previous hash (or nil)
            val prevHash = if r.hasNull then
                r.readNull()
                None
            else Some(Hash32.given_Decoder_Hash32.read(r))

            val issuerVkey = ByteString.unsafeFromArray(r.readBytes())
            val vrfVkey = ByteString.unsafeFromArray(r.readBytes())
            val vrfResult = VrfCert.given_Decoder_VrfCert.read(r)
            val blockBodySize = r.readLong()
            val blockBodyHash = Hash32.given_Decoder_Hash32.read(r)
            val operationalCert = OperationalCert.given_Decoder_OperationalCert.read(r)
            val protocolVersion = ProtocolVersion.given_Decoder_ProtocolVersion.read(r)

            // Validate fields
            if blockNumber < 0 then
                r.validationFailure(s"Block number must be non-negative, got $blockNumber")
            if slot < 0 then r.validationFailure(s"Slot must be non-negative, got $slot")
            if issuerVkey.size != 32 then
                r.validationFailure(
                  s"Issuer verification key must be 32 bytes, got ${issuerVkey.size}"
                )
            if vrfVkey.size != 32 then
                r.validationFailure(s"VRF verification key must be 32 bytes, got ${vrfVkey.size}")
            if blockBodySize < 0 then
                r.validationFailure(s"Block body size must be non-negative, got $blockBodySize")

            HeaderBody(
              blockNumber,
              slot,
              prevHash,
              issuerVkey,
              vrfVkey,
              vrfResult,
              blockBodySize,
              blockBodyHash,
              operationalCert,
              protocolVersion
            )
