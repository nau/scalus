package scalus.ledger

import scalus.builtin.ByteString
import io.bullet.borer.{Codec, Decoder, Encoder, Reader, Writer}
import io.bullet.borer.derivation.ArrayBasedCodecs.*

/** Represents a 28-byte hash value used in Cardano
  *
  * Hash28 is commonly used for address key hashes and script hashes
  */
case class Hash28(bytes: ByteString) derives Codec {

    /** Ensures the hash is exactly 28 bytes */
    require(bytes.size == 28, s"Hash28 must be 28 bytes, got ${bytes.size}")
}

object Hash28 {

    /** Create a Hash28 from a hex string */
    def fromHex(hex: String): Hash28 = {
        val bytes = ByteString.fromHex(hex)
        require(bytes.size == 28, s"Hash28 must be 28 bytes, got ${bytes.size}")
        Hash28(bytes)
    }
}

/** Represents a 32-byte hash value used in Cardano
  *
  * Hash32 is commonly used for transaction IDs, block hashes and other crypto hashes
  */
final case class Hash32(bytes: ByteString) {

    /** Ensures the hash is exactly 32 bytes */
    require(bytes.size == 32, s"Hash32 must be 32 bytes, got ${bytes.size}")
}

object Hash32 {

    /** Create a Hash32 from a hex string */
    def fromHex(hex: String): Hash32 = {
        val bytes = ByteString.fromHex(hex)
        require(bytes.size == 32, s"Hash32 must be 32 bytes, got ${bytes.size}")
        Hash32(bytes)
    }

    /** Create a nil (empty/zero) Hash32 - used for special cases */
    val nil: Hash32 = Hash32(ByteString.fill(32, 0))

    /** CBOR encoder for Hash32 */
    given Encoder[Hash32] = Encoder { (w, hash) =>
        w.writeBytes(hash.bytes.bytes)
    }

    /** CBOR decoder for Hash32 */
    given Decoder[Hash32] = Decoder { r =>
        val bytes = ByteString.unsafeFromArray(r.readBytes())
        require(bytes.size == 32, s"Hash32 must be 32 bytes, got ${bytes.size}")
        Hash32(bytes)
    }
}

/** Represents an amount of Cardano's native currency (ADA)
  *
  * In Cardano, coins are represented as unsigned integers
  */
final case class Coin(value: Long) {

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

    /** CBOR encoder for Coin */
    given Encoder[Coin] = Encoder { (w, coin) =>
        w.writeLong(coin.value)
    }

    /** CBOR decoder for Coin */
    given Decoder[Coin] = Decoder { r =>
        val value = r.readLong()
        require(value >= 0, s"Coin value must be non-negative, got $value")
        Coin(value)
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
final case class ScriptHash(bytes: ByteString) {

    /** Ensures the hash is exactly 28 bytes */
    require(bytes.size == 28, s"ScriptHash must be 28 bytes, got ${bytes.size}")
}

object ScriptHash {

    /** Create a ScriptHash from a hex string */
    def fromHex(hex: String): ScriptHash = {
        val bytes = ByteString.fromHex(hex)
        require(bytes.size == 28, s"ScriptHash must be 28 bytes, got ${bytes.size}")
        ScriptHash(bytes)
    }

    /** CBOR encoder for ScriptHash */
    given Encoder[ScriptHash] = Encoder { (w, hash) =>
        w.writeBytes(hash.bytes.bytes)
    }

    /** CBOR decoder for ScriptHash */
    given Decoder[ScriptHash] = Decoder { r =>
        val bytes = ByteString.unsafeFromArray(r.readBytes())
        require(bytes.size == 28, s"ScriptHash must be 28 bytes, got ${bytes.size}")
        ScriptHash(bytes)
    }
}

type PolicyId = ScriptHash

/** Represents an asset name in Cardano's multi-asset framework
  *
  * Asset names can be between 0 and 32 bytes long.
  */
final case class AssetName(bytes: ByteString) {

    /** Ensures the asset name is at most 32 bytes */
    require(bytes.size <= 32, s"AssetName must be at most 32 bytes, got ${bytes.size}")

    /** Convert to ASCII string if possible, otherwise returns hex representation */
    def asString: String = {
        if (bytes.bytes.forall(b => b >= 32 && b < 127)) {
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

    /** CBOR encoder for AssetName */
    given Encoder[AssetName] = Encoder { (w, assetName) =>
        w.writeBytes(assetName.bytes.bytes)
    }

    /** CBOR decoder for AssetName */
    given Decoder[AssetName] = Decoder { r =>
        val bytes = ByteString.unsafeFromArray(r.readBytes())
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

/** Represents a Cardano address
  *
  * Cardano addresses encode information in their header byte:
  *   - bit 7: 0 for Shelley addresses, 1 for special addresses
  *   - bits 6-5: address type
  *   - bit 4: credential type (key hash or script hash)
  *   - bits 3-0: network ID
  *
  * Address types for Shelley addresses (bit 7 = 0):
  *   - 00: base address (payment key + stake key)
  *   - 01: pointer address (payment key + stake pointer)
  *   - 10: enterprise address (payment key only)
  *   - 11: reward address (stake key only)
  *
  * Reward addresses (bits 7-5 = 111):
  *   - bit 4: credential type
  *   - bits 3-0: network ID
  *
  * Byron addresses (bits 7-4 = 1000)
  */
final case class Address(bytes: ByteString) {

    /** Get the network ID from the address */
    def networkId: Int = bytes(0) & 0x0f

    /** Is this a testnet address? */
    def isTestnet: Boolean = networkId != 1

    /** Is this a mainnet address? */
    def isMainnet: Boolean = networkId == 1

    /** Get bech32 representation (for human-readable format) */
    def toBech32: String = {
        // In a real implementation, this would convert to bech32 format
        // For example: addr1q9qfllpxg2vu4lq6rnpel4pvpp5xnv3kvvgtxk6k6wp4ff89xrhu8jnu3pj9rqlmnfxm0husrvdkfhxrt5nazvcmhcssk5m9rq
        "addr1..." + bytes.toHex.take(8)
    }
}

object Address {

    /** Create an Address from a hex string */
    def fromHex(hex: String): Address = Address(ByteString.fromHex(hex))

    /** Create an Address from bech32 string */
    def fromBech32(bech32: String): Address = {
        // In a real implementation, this would parse from bech32 format
        // For this example, we'll create a dummy address
        Address(ByteString.fromHex("00"))
    }

    /** CBOR encoder for Address */
    given Encoder[Address] = Encoder { (w, address) =>
        w.writeBytes(address.bytes.bytes)
    }

    /** CBOR decoder for Address */
    given Decoder[Address] = Decoder { r =>
        Address(ByteString.unsafeFromArray(r.readBytes()))
    }
}

/** Represents a slot number in Cardano
  *
  * A slot is a fixed period of time in the Cardano blockchain. Slots are where blocks can be added
  * to the chain.
  */
final case class Slot(slot: Long) {

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

object Slot {

    /** CBOR encoder for Slot */
    given Encoder[Slot] = Encoder { (w, slot) =>
        w.writeLong(slot.slot)
    }

    /** CBOR decoder for Slot */
    given Decoder[Slot] = Decoder { r =>
        val slot = r.readLong()
        require(slot >= 0, s"Slot number must be non-negative, got $slot")
        Slot(slot)
    }
}

/** Represents a set of key hashes that must sign the transaction
  *
  * This provides a way to ensure specific keys are required to sign a transaction, even if they're
  * not otherwise required by the transaction's inputs or outputs.
  */
final case class RequiredSigners(keyHashes: Set[Hash28])

object RequiredSigners {

    /** CBOR encoder for RequiredSigners */
    given Encoder[RequiredSigners] = Encoder { (w, signers) =>
        // Encode as a definite-length array
        w.writeArrayHeader(signers.keyHashes.size)
        signers.keyHashes.foreach { keyHash =>
            w.write(keyHash)
        }
        w
    }

    /** CBOR decoder for RequiredSigners */
//    given Decoder[RequiredSigners] = Decoder { r =>
//        val count = r.readArrayHeader()
//        val keyHashes = for (_ <- 0L until count) yield r.read[Hash28]
//        RequiredSigners(keyHashes.toSet)
//    }
}

/** Represents a hash of the auxiliary data (transaction metadata)
  *
  * This is simply a type alias for Hash32 to provide better type safety and readability.
  */
type AuxiliaryDataHash = Hash32

/** Represents a hash of the script data
  *
  * This hash includes the redeemers, datums, and language views from the transaction. It's used to
  * ensure the script's execution environment is consistent with what was intended.
  *
  * This is simply a type alias for Hash32 to provide better type safety and readability.
  */
type ScriptDataHash = Hash32

/** Represents execution units for Plutus scripts in Cardano */
case class ExUnits(
    /** Memory units */
    mem: Long,

    /** CPU step units */
    steps: Long
) derives Codec:
    require(mem >= 0, s"Memory units must be non-negative, got $mem")
    require(steps >= 0, s"Step units must be non-negative, got $steps")
