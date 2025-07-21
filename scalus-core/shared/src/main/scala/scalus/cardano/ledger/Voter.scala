package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.derivation.key
import scalus.builtin.ByteString

/** Represents voting options in the Cardano blockchain governance system.
  *
  * A vote can be one of three values:
  *   - No: Against the proposal
  *   - Yes: In favor of the proposal
  *   - Abstain: Neither for nor against the proposal
  */
enum Vote {
    case No, Yes, Abstain
}

object Vote {

    /** CBOR Encoder for Vote. Encodes as an unsigned integer (0, 1, or 2).
      */
    given Encoder[Vote] = (w: Writer, value: Vote) => w.writeInt(value.ordinal)

    /** CBOR Decoder for Vote. Decodes from an unsigned integer (0, 1, or 2).
      */
    given Decoder[Vote] = (r: Reader) => Vote.fromOrdinal(r.readInt())
}

/** Represents a voter in the Cardano blockchain governance system.
  *
  * A voter can be one of several types:
  *   - ConstitutionalCommitteeHotKey: A hot key of a constitutional committee member
  *   - ConstitutionalCommitteeHotScript: A script for a constitutional committee
  *   - DRepKey: A delegated representative key
  *   - DRepScript: A delegated representative script
  *   - StakingPoolKey: A staking pool key
  */
enum Voter derives Codec.All {

    /** A constitutional committee member's hot key.
      *
      * @param keyHash
      *   The key hash of the committee member
      */
    @key(0) case ConstitutionalCommitteeHotKey(keyHash: AddrKeyHash)

    /** A constitutional committee script.
      *
      * @param scriptHash
      *   The hash of the script
      */
    @key(1) case ConstitutionalCommitteeHotScript(scriptHash: ScriptHash)

    /** A delegated representative's key.
      *
      * @param keyHash
      *   The key hash of the DRep
      */
    @key(2) case DRepKey(keyHash: AddrKeyHash)

    /** A delegated representative script.
      *
      * @param scriptHash
      *   The hash of the script
      */
    @key(3) case DRepScript(scriptHash: ScriptHash)

    /** A staking pool's key.
      *
      * @param keyHash
      *   The key hash of the pool
      */
    @key(4) case StakingPoolKey(keyHash: AddrKeyHash)
}

object Voter {

    given Ordering[Voter] = Ordering.by {
        case ConstitutionalCommitteeHotKey(keyHash)       => (0, keyHash: ByteString)
        case ConstitutionalCommitteeHotScript(scriptHash) => (1, scriptHash: ByteString)
        case DRepKey(keyHash)                             => (2, keyHash: ByteString)
        case DRepScript(scriptHash)                       => (3, scriptHash: ByteString)
        case StakingPoolKey(keyHash)                      => (4, keyHash: ByteString)
    }
}
