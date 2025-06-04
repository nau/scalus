package scalus.cardano.ledger

import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.{Cbor, Codec, Decoder, Encoder, Writer}
import scalus.builtin.ByteString
import io.bullet.borer.NullOptions.given
import scalus.cardano.address.Address

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
    trait TransactionHash
    trait BlockHash
}

opaque type Hash[+HashFunction, +Purpose] <: ByteString = ByteString
object Hash {
    type KeyHash = Hash[Blake2b_224, HashPurpose.KeyHash]
    type ScriptHash = Hash[Blake2b_224, HashPurpose.ScriptHash]

    def apply[HF: HashSize, Purpose](bytes: ByteString): Hash[HF, Purpose] = {
        val size = summon[HashSize[HF]].size
        require(bytes.size == size, s"Hash must be $size bytes, got ${bytes.size}")
        bytes
    }

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
type DataHash = Hash[Blake2b_224, HashPurpose.DataHash]
type MetadataHash = Hash[Blake2b_256, HashPurpose.MetadataHash]
type TransactionHash = Hash[Blake2b_256, HashPurpose.TransactionHash]
type BlockHash = Hash[Blake2b_256, HashPurpose.BlockHash]
type AuxiliaryDataHash = Hash[Blake2b_256, HashPurpose.AuxiliaryDataHash]
type VrfKeyHash = Hash[Blake2b_256, HashPurpose.VrfKeyHash]

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
case class Hash28(bytes: ByteString) derives Codec {

    /** Ensures the hash is exactly 28 bytes */
    require(bytes.size == 28, s"Hash28 must be 28 bytes, got ${bytes.size}")
}

object Hash28 {

    val Size = 28

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

/** Represents an amount of Cardano's native currency (ADA)
  *
  * In Cardano, coins are represented as unsigned integers
  */
final case class Coin(value: Long) derives Codec {

    /** Ensures the coin value is non-negative */
    require(value >= 0, s"Coin value must be non-negative, got $value")

    /** Add another coin amount */
    def +(other: Coin): Coin = Coin(value + other.value)

    /** Subtract another coin amount, returns 0 if the result would be negative */
    def -(other: Coin): Coin = Coin(math.max(0, value - other.value))
}

object Coin {

    /** Zero coin value */
    val zero: Coin = Coin(0)
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
final case class ScriptHash(hash: Hash28) derives Codec

object ScriptHash {

    /** Create a ScriptHash from a hex string */
    def fromHex(hex: String): ScriptHash = {
        ScriptHash(Hash28.fromHex(hex))
    }
}

type PolicyHash = ScriptHash
type PolicyId = ScriptHash

type MultiAsset[A] = Map[PolicyId, Map[AssetName, A]]

type Mint = MultiAsset[Long]

/** Represents an asset name in Cardano's multi-asset framework
  *
  * Asset names can be between 0 and 32 bytes long.
  */
final case class AssetName(bytes: ByteString) derives Codec {

    /** Ensures the asset name is at most 32 bytes */
    require(bytes.size <= 32, s"AssetName must be at most 32 bytes, got ${bytes.size}")

    /** Convert to ASCII string if possible, otherwise returns hex representation */
    def asString: String = {
        if bytes.bytes.forall(b => b >= 32 && b < 127) then {
            new String(bytes.bytes, "ASCII")
        } else {
            bytes.toHex
        }
    }
}

object AssetName {

    /** Empty asset name */
    val empty: AssetName = AssetName(ByteString.empty)

    /** Create an AssetName from a hex string */
    def fromHex(hex: String): AssetName = {
        val bytes = ByteString.fromHex(hex)
        require(bytes.size <= 32, s"AssetName must be at most 32 bytes, got ${bytes.size}")
        AssetName(bytes)
    }

    /** Create an AssetName from a UTF-8 string */
    def fromString(str: String): AssetName = {
        val bytes = ByteString.fromString(str)
        require(bytes.size <= 32, s"AssetName must be at most 32 bytes, got ${bytes.size}")
        AssetName(bytes)
    }
}

/** Represents the supported scripting languages in Cardano */
enum Language {

    /** Plutus V1, first version of Plutus */
    case PlutusV1

    /** Plutus V2, introduced in Vasil hard fork */
    case PlutusV2

    /** Plutus V3, introduced in Conway hard fork */
    case PlutusV3
}

object Language {

    /** Gets the language ID (used in CBOR encoding) */
    def languageId(language: Language): Int = language match {
        case Language.PlutusV1 => 0
        case Language.PlutusV2 => 1
        case Language.PlutusV3 => 2
    }

    /** Gets the language from an ID */
    def fromId(id: Int): Language = id match {
        case 0 => Language.PlutusV1
        case 1 => Language.PlutusV2
        case 2 => Language.PlutusV3
        case _ => throw new IllegalArgumentException(s"Unknown language ID: $id")
    }

    /** CBOR encoder for Language */
    given Encoder[Language] = Encoder { (w, language) =>
        w.writeInt(languageId(language))
    }

    /** CBOR decoder for Language */
    given Decoder[Language] = Decoder { r =>
        fromId(r.readInt())
    }
}

/** Represents a Cardano address bytes */
// TODO: consider using Array[Byte] instead of ByteString
opaque type AddressBytes <: ByteString = ByteString

object AddressBytes {
    inline def apply(bytes: ByteString): AddressBytes = bytes

    /** Create an Address from a hex string */
    def fromHex(hex: String): AddressBytes = ByteString.fromHex(hex)

    /** Create an Address from bech32 string */
    def fromBech32(bech32: String): AddressBytes = {
        ByteString.unsafeFromArray(Bech32.decode(bech32).data)
    }

    extension (self: AddressBytes) {
        def toAddress: Address = Address.fromByteString(self)
    }

    given Encoder[AddressBytes] = Encoder { (w, address) =>
        // here we explicitly pass the ByteString encoder to avoid StackOverflowError
        // because here Encoder[Address] is resolved as Encoder[ByteString]
        w.write[ByteString](address)(using ByteString.given_Encoder_ByteString)
    }

    given Decoder[AddressBytes] = Decoder { r =>
        // here we explicitly pass the ByteString decoder to avoid StackOverflowError
        // because here Decoder[Address] is resolved as Decoder[ByteString]
        val bytes = r.read[ByteString]()(using ByteString.given_Decoder_ByteString)
        AddressBytes(bytes)
    }
}

/** Represents a slot number in Cardano
  *
  * A slot is a fixed period of time in the Cardano blockchain. Slots are where blocks can be added
  * to the chain.
  */
final case class Slot(slot: Long) derives Codec {

    /** Ensures the slot number is non-negative */
    require(slot >= 0, s"Slot number must be non-negative, got $slot")

    /** Compare slots for ordering */
    def <=(other: Slot): Boolean = slot <= other.slot

    /** Compare slots for ordering */
    def >=(other: Slot): Boolean = slot >= other.slot

    /** Compare slots for ordering */
    def <(other: Slot): Boolean = slot < other.slot

    /** Compare slots for ordering */
    def >(other: Slot): Boolean = slot > other.slot
}

/** Represents execution units for Plutus scripts in Cardano */
case class ExUnits(
    /** Memory units */
    mem: Long,

    /** CPU step units */
    steps: Long
) derives Codec:
    require(mem >= 0, s"Memory units must be non-negative, got $mem")
    require(steps >= 0, s"Step units must be non-negative, got $steps")

/** Represents execution unit prices in the Cardano blockchain.
  *
  * ExUnitPrices define the cost of execution units in terms of the protocol's currency, with
  * separate prices for memory usage and CPU steps.
  *
  * @param memPrice
  *   Price per memory unit
  * @param stepPrice
  *   Price per step unit
  */
case class ExUnitPrices(
    memPrice: NonNegativeInterval,
    stepPrice: NonNegativeInterval
) derives Codec

/** Represents cost models for script languages in the Cardano blockchain.
  *
  * Cost models define the execution costs of operations in different script languages. The keys are
  * language identifiers (0 for Plutus V1, 1 for Plutus V2, 2 for Plutus V3), and the values are
  * lists of integers representing operation costs.
  *
  * @param models
  *   Map from language identifiers to lists of operation costs
  */
case class CostModels(models: Map[Int, List[Long]]) derives Codec

/** Represents a constitution in the Cardano blockchain governance system.
  *
  * A constitution consists of an anchor (pointing to the constitution text) and an optional script
  * hash for the constitution script.
  *
  * @param anchor
  *   The anchor pointing to the constitution text
  * @param scriptHash
  *   Optional script hash for the constitution script
  */
case class Constitution(
    anchor: Anchor,
    scriptHash: Option[ScriptHash]
) derives Codec

opaque type OriginalCborByteArray[A] <: Array[Byte] = Array[Byte]
object OriginalCborByteArray {

    /** Create an OriginalCborByteArray from a byte array */
    def apply[A](bytes: Array[Byte]): OriginalCborByteArray[A] = bytes
}

case class KeepRaw[A](value: A, raw: Array[Byte])

object KeepRaw {
    def apply[A: Encoder](value: A): KeepRaw[A] = new KeepRaw(value, Cbor.encode(value).toByteArray)

    given [A: Decoder: OriginalCborByteArray]: Decoder[KeepRaw[A]] =
        Decoder { r =>
            val start = r.cursor
            val value = r.read[A]()
            // Here we need to call `dataItem()` to ensure the cursor is updated
            // see https://github.com/sirthias/borer/issues/761#issuecomment-2919035884
            r.dataItem()
            val end = r.cursor
            val raw = summon[OriginalCborByteArray[A]].slice(start.toInt, end.toInt)
            KeepRaw(value, raw)
        }

    given [A: Encoder]: Encoder[KeepRaw[A]] = (w: Writer, value: KeepRaw[A]) => {
        summon[Encoder[A]].write(w, value.value)
    }
}
