package scalus.cardano.ledger

import io.bullet.borer.Codec
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.derivation.key

/** Represents a Delegated Representative (DRep) in the Cardano blockchain.
  *
  * A DRep can be one of several types:
  *   - KeyHash: A DRep identified by a key hash
  *   - ScriptHash: A DRep identified by a script hash
  *   - AlwaysAbstain: A special DRep that always abstains from voting
  *   - AlwaysNoConfidence: A special DRep that always votes no confidence
  */
enum DRep derives Codec.All:

    /** A DRep identified by a key hash.
      *
      * @param keyHash
      *   The key hash of the DRep
      */
    @key(0) case KeyHash(keyHash: AddrKeyHash)

    /** A DRep identified by a script hash.
      *
      * @param scriptHash
      *   The script hash of the DRep
      */
    @key(1) case ScriptHash(scriptHash: scalus.cardano.ledger.ScriptHash)

    /** A special DRep that always abstains from voting.
      */
    @key(2) case AlwaysAbstain

    /** A special DRep that always votes no confidence.
      */
    @key(3) case AlwaysNoConfidence
