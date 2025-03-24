package scalus.ledger

import scalus.builtin.ByteString
import io.bullet.borer.{Decoder, Encoder, Reader, Writer}

/** Represents a block header in Cardano */
case class Header(
    /** Header body with block metadata */
    headerBody: HeaderBody,

    /** Body signature (KES signature, 448 bytes) */
    bodySignature: ByteString
):
    require(
      bodySignature.size == 448,
      s"Body signature must be 448 bytes, got ${bodySignature.size}"
    )

    /** Get block number */
    def blockNumber: Long = headerBody.blockNumber

    /** Get slot number */
    def slot: Long = headerBody.slot

    /** Get previous block hash */
    def prevHash: Option[Hash32] = headerBody.prevHash

object Header:
    /** CBOR encoder for Header */
    given Encoder[Header] with
        def write(w: Writer, value: Header): Writer =
            w.writeArrayHeader(2)
            HeaderBody.given_Encoder_HeaderBody.write(w, value.headerBody)
            w.writeBytes(value.bodySignature.bytes)
            w

    /** CBOR decoder for Header */
    given Decoder[Header] with
        def read(r: Reader): Header =
            val size = r.readArrayHeader()
            if size != 2 then r.validationFailure(s"Expected 2 elements for Header, got $size")

            val headerBody = HeaderBody.given_Decoder_HeaderBody.read(r)
            val bodySignature = ByteString.unsafeFromArray(r.readBytes())

            if bodySignature.size != 448 then
                r.validationFailure(s"Body signature must be 448 bytes, got ${bodySignature.size}")

            Header(headerBody, bodySignature)
