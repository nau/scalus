package scalus.ledger

import io.bullet.borer.{Decoder, Encoder, Reader, Writer}
import scalus.builtin.ByteString

/** Represents a key hash used in addresses in the Cardano blockchain.
  *
  * An AddrKeyHash is a 28-byte hash value that identifies a public key and is used in constructing
  * addresses.
  *
  * @param bytes
  *   The 28-byte hash value
  */
case class AddrKeyHash(bytes: ByteString) {
    // Ensure that the key hash is exactly 28 bytes
    require(bytes.size == 28, s"AddrKeyHash must be exactly 28 bytes, got ${bytes.size}")
}

object AddrKeyHash {
    // Companion object with Encoder/Decoder implementations

    /** Creates an AddrKeyHash from a hex string representation.
      *
      * @param hex
      *   A hex string representing a 28-byte value
      * @return
      *   The corresponding AddrKeyHash
      */
    def fromHex(hex: String): AddrKeyHash =
        AddrKeyHash(ByteString.fromHex(hex))

    /** CBOR Encoder for AddrKeyHash. Encodes as a bytestring of 28 bytes.
      */
    given Encoder[AddrKeyHash] = new Encoder[AddrKeyHash] {
        def write(w: Writer, value: AddrKeyHash): Writer =
            w.writeBytes(value.bytes.bytes)
    }

    /** CBOR Decoder for AddrKeyHash. Decodes from a bytestring of 28 bytes.
      */
    given Decoder[AddrKeyHash] = (r: Reader) =>
        val bytes = ByteString.unsafeFromArray(r.readBytes())
        AddrKeyHash(bytes)
}
