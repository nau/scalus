package scalus.cardano.ledger

import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.*

/** Represents metadata for a stake pool in the Cardano blockchain.
  *
  * Pool metadata provides information about the pool such as name, description, ticker symbol, etc.
  * It's stored off-chain, and only the URL and hash are included in the blockchain.
  *
  * @param url
  *   URL where the metadata can be found
  * @param metadataHash
  *   Hash of the metadata for verification
  */
case class PoolMetadata(url: String, metadataHash: MetadataHash) derives Codec {
    // Validate URL length
    require(url.length <= 128, s"URL must be at most 128 characters, got ${url.length}")
}
