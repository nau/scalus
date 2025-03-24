package scalus.ledger

import io.bullet.borer.{Decoder, Encoder, Reader, Writer}
import scalus.builtin.ByteString

/** Represents an anchor in the Cardano blockchain.
  *
  * An anchor contains a URL and a data hash that can be used to verify the data retrieved from the
  * URL.
  *
  * @param url
  *   The URL where the data can be retrieved
  * @param dataHash
  *   A 32-byte hash of the data
  */
case class Anchor(url: String, dataHash: Hash32):
    /** Validate the URL length */
    require(url.length <= 128, s"Anchor URL must be at most 128 characters, got ${url.length}")

    override def toString: String = s"Anchor($url, ${dataHash.bytes.toHex})"

object Anchor:
    /** CBOR encoder for Anchor */
    given Encoder[Anchor] with
        def write(w: Writer, value: Anchor): Writer =
            w.writeArrayHeader(2)
                .writeString(value.url)
                .writeBytes(value.dataHash.bytes.bytes)

    /** CBOR decoder for Anchor */
    given Decoder[Anchor] with
        def read(r: Reader): Anchor =
            r.readArrayHeader()
            val url = r.readString()
            if url.length > 128 then
                r.validationFailure(s"Anchor URL must be at most 128 characters, got ${url.length}")

            val hashBytes = ByteString.unsafeFromArray(r.readBytes())
            val dataHash = Hash32(hashBytes)

            Anchor(url, dataHash)
