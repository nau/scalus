package scalus.cardano.ledger

import io.bullet.borer.{Decoder, Encoder}
import scalus.builtin.ByteString

trait Blake2b_256
trait Blake2b_224

case class HashSize[HF](size: Int)

object HashSize {
    given HashSize[Blake2b_224] = HashSize(28)
    given HashSize[Blake2b_256] = HashSize(32)
}

object HashPurpose {
    trait KeyHash
    trait ScriptHash
    trait DataHash
    trait ScriptDataHash
    trait MetadataHash
    trait AuxiliaryDataHash
    trait VrfKeyHash
    trait PoolKeyHash
    trait StakeKeyHash
    trait TransactionHash
    trait BlockHash
}

opaque type Hash[+HashFunction, +Purpose] <: ByteString = ByteString
object Hash {
    type ScriptHash = Hash[Blake2b_224, HashPurpose.ScriptHash]

    def apply[HF: HashSize, Purpose](bytes: ByteString): Hash[HF, Purpose] = {
        val size = summon[HashSize[HF]].size
        require(bytes.size == size, s"Hash must be $size bytes, got ${bytes.size}")
        bytes
    }

    def stakeKeyHash(bytes: ByteString): StakeKeyHash = Hash(bytes)
    def scriptHash(bytes: ByteString): ScriptHash = Hash(bytes)

    given Encoder[HF, Purpose]: Encoder[Hash[HF, Purpose]] = { (w, hash) =>
        // here we explicitly pass the ByteString encoder to avoid StackOverflowError
        // because here Encoder[Hash[]] is resolved as Encoder[ByteString]
        w.write[ByteString](hash)(using ByteString.given_Encoder_ByteString)
    }

    given Decoder[HF: HashSize, Purpose]: Decoder[Hash[HF, Purpose]] = { r =>
        // here we explicitly pass the ByteString decoder to avoid StackOverflowError
        // because here Decoder[Hash[]] is resolved as Decoder[ByteString]
        val bytes = r.read[ByteString]()(using ByteString.given_Decoder_ByteString)
        Hash[HF, Purpose](bytes)
    }
}

type AnyHash = Hash[Any, Any]

/** Represents a key hash used in addresses in the Cardano blockchain.
  *
  * An AddrKeyHash is a 28-byte hash value that identifies a public key and is used in constructing
  * addresses.
  *
  * @param hash
  *   The 28-byte hash value
  */
//case class AddrKeyHash(hash: Hash28) derives Codec
type AddrKeyHash = Hash[Blake2b_224, HashPurpose.KeyHash]
object AddrKeyHash {

    def apply(bytes: ByteString): AddrKeyHash = {
        require(bytes.size == 28, s"AddrKeyHash must be 28 bytes, got ${bytes.size}")
        Hash[Blake2b_224, HashPurpose.KeyHash](bytes)
    }

    /** Create an AddrKeyHash from a hex string */
    def fromHex(hex: String): AddrKeyHash = {
        val bytes = ByteString.fromHex(hex)
        Hash[Blake2b_224, HashPurpose.KeyHash](bytes)
    }
}

/** Represents a script hash in Cardano
  *
  * To compute a script hash, a tag must be prepended to the script bytes before hashing. The tag is
  * determined by the script language:
  *   - "\x00" for multisig scripts
  *   - "\x01" for Plutus V1 scripts
  *   - "\x02" for Plutus V2 scripts
  *   - "\x03" for Plutus V3 scripts
  */
type ScriptHash = Hash[Blake2b_224, HashPurpose.ScriptHash]
object ScriptHash {

    /** Create a ScriptHash from a hex string */
    def fromHex(hex: String): ScriptHash = {
        Hash(Hash28.fromHex(hex))
    }
}
type DataHash = Hash[Blake2b_224, HashPurpose.DataHash]
type MetadataHash = Hash[Blake2b_256, HashPurpose.MetadataHash]
type TransactionHash = Hash[Blake2b_256, HashPurpose.TransactionHash]
type BlockHash = Hash[Blake2b_256, HashPurpose.BlockHash]
type AuxiliaryDataHash = Hash[Blake2b_256, HashPurpose.AuxiliaryDataHash]
type VrfKeyHash = Hash[Blake2b_256, HashPurpose.VrfKeyHash]
type PoolKeyHash = Hash[Blake2b_224, HashPurpose.PoolKeyHash]
type StakeKeyHash = Hash[Blake2b_224, HashPurpose.StakeKeyHash]

/** Represents a hash of the script data
  *
  * This hash includes the redeemers, datums, and language views from the transaction. It's used to
  * ensure the script's execution environment is consistent with what was intended.
  *
  * This is simply a type alias for Hash32 to provide better type safety and readability.
  */
type ScriptDataHash = Hash[Blake2b_256, HashPurpose.ScriptDataHash]

/** Represents a 28-byte hash value used in Cardano
  *
  * Hash28 is commonly used for address key hashes and script hashes
  */
//

type Hash28 = Hash[Blake2b_224, Any]

object Hash28 {

    def apply(bytes: ByteString): Hash28 = {
        Hash[Blake2b_224, Any](bytes)
    }

    /** Create a Hash28 from a hex string */
    def fromHex(hex: String): Hash28 = {
        val bytes = ByteString.fromHex(hex)
        Hash28(bytes)
    }
}

type Hash32 = Hash[Blake2b_256, Any]

object Hash32 {

    /** Create a Hash32 from a hex string */
    def fromHex(hex: String): Hash32 = {
        val bytes = ByteString.fromHex(hex)
        Hash(bytes)
    }
}

type PolicyHash = ScriptHash
type PolicyId = ScriptHash
