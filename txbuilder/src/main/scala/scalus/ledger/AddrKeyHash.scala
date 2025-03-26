package scalus.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import scalus.builtin.ByteString

/** Represents a key hash used in addresses in the Cardano blockchain.
  *
  * An AddrKeyHash is a 28-byte hash value that identifies a public key and is used in constructing
  * addresses.
  *
  * @param bytes
  *   The 28-byte hash value
  */
case class AddrKeyHash(bytes: ByteString) derives Codec {
    // Ensure that the key hash is exactly 28 bytes
    require(bytes.size == 28, s"AddrKeyHash must be exactly 28 bytes, got ${bytes.size}")
}
