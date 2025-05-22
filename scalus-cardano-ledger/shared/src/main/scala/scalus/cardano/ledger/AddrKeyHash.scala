package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*

/** Represents a key hash used in addresses in the Cardano blockchain.
  *
  * An AddrKeyHash is a 28-byte hash value that identifies a public key and is used in constructing
  * addresses.
  *
  * @param hash
  *   The 28-byte hash value
  */
case class AddrKeyHash(hash: Hash28) derives Codec
