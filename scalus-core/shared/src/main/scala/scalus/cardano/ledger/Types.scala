package scalus.cardano.ledger

import io.bullet.borer.NullOptions.given
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.*
import scalus.builtin.{platform, ByteString, Data}
import scalus.cardano.address.Address
import scalus.utils.Hex.toHex
import upickle.default.ReadWriter as UpickleReadWriter

import java.util
import scala.collection.immutable.{SortedMap, TreeMap}
import scala.compiletime.asMatchable

enum Era(val value: Int) extends Enumeration {
    case Babbage extends Era(6)
    case Conway extends Era(7)
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

    def >(other: Coin): Boolean = value > other.value
    def >=(other: Coin): Boolean = value >= other.value
    def <(other: Coin): Boolean = value < other.value
    def <=(other: Coin): Boolean = value <= other.value
}

object Coin {

    /** Zero coin value */
    val zero: Coin = Coin(0)
}

/** Minting MultiAsset. Can't contain zeros, can't be empty */
opaque type Mint <: MultiAsset = MultiAsset
object Mint {
    def apply(ma: MultiAsset): Mint = {
        require(!ma.isEmpty, "Mint cannot be empty")
        require(
          ma.assets.forall { case (_, assets) => assets.forall { case (_, value) => value != 0 } },
          "Mint cannot contain zero values"
        )
        ma
    }

    given Encoder[Mint] = MultiAsset.given_Encoder_MultiAsset
    given Decoder[Mint] = MultiAsset.given_Decoder_MultiAsset.map(Mint.apply)
}

case class MultiAsset(assets: SortedMap[PolicyId, SortedMap[AssetName, Long]]) {
    def isEmpty: Boolean = assets.isEmpty
}

object MultiAsset {
    val zero: MultiAsset = MultiAsset(SortedMap.empty)
    val empty: MultiAsset = zero
    def binOp(op: (Long, Long) => Long)(self: MultiAsset, other: MultiAsset): MultiAsset = {
        val assets: SortedMap[PolicyId, SortedMap[AssetName, Long]] =
            (self.assets.keySet ++ other.assets.keySet).view
                .map { policyId =>
                    val selfAssets =
                        self.assets.getOrElse(policyId, SortedMap.empty[AssetName, Long])
                    val otherAssets =
                        other.assets.getOrElse(policyId, SortedMap.empty[AssetName, Long])

                    val mergedAssets: SortedMap[AssetName, Long] =
                        (selfAssets.keySet ++ otherAssets.keySet).view
                            .map { assetName =>
                                val combinedValue =
                                    op(
                                      selfAssets.getOrElse(assetName, 0L),
                                      otherAssets.getOrElse(assetName, 0L)
                                    )
                                assetName -> combinedValue
                            }
                            .to(TreeMap)
                    policyId -> mergedAssets
                }
                .to(TreeMap)
        MultiAsset(assets)
    }

    extension (self: MultiAsset) {
        def +(other: MultiAsset): MultiAsset = binOp(_ + _)(self, other)
        def -(other: MultiAsset): MultiAsset = binOp(_ - _)(self, other)
    }

    given Encoder[MultiAsset] =
        Encoder.forMap[PolicyId, SortedMap[AssetName, Long], SortedMap].contramap(_.assets)

    given Decoder[MultiAsset] = Decoder { r =>
        given Decoder[TreeMap[AssetName, Long]] = Decoder.forTreeMap[AssetName, Long]
        Decoder.forTreeMap[PolicyId, TreeMap[AssetName, Long]].map(MultiAsset.apply).read(r)
    }

}

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

    given Ordering[AssetName] = (x: AssetName, y: AssetName) => {
        Ordering[ByteString].compare(x.bytes, y.bytes)
    }

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

    given Ordering[Language] = new Ordering[Language] {
        def compare(x: Language, y: Language): Int = {
            Language.languageId(x) - Language.languageId(y)
        }
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
    memory: Long,

    /** CPU step units */
    steps: Long
) derives Codec,
      UpickleReadWriter:
    require(memory >= 0, s"Memory units must be non-negative, got $memory")
    require(steps >= 0, s"Step units must be non-negative, got $steps")

    def +(other: ExUnits): ExUnits =
        ExUnits(memory + other.memory, steps + other.steps)

object ExUnits:
    val zero: ExUnits = ExUnits(0, 0)

/** Represents execution unit prices in the Cardano blockchain.
  *
  * ExUnitPrices define the cost of execution units in terms of the protocol's currency, with
  * separate prices for memory usage and CPU steps.
  *
  * @param priceMemory
  *   Price per memory unit
  * @param priceSteps
  *   Price per step unit
  */
case class ExUnitPrices(
    priceMemory: NonNegativeInterval,
    priceSteps: NonNegativeInterval
) derives Codec,
      UpickleReadWriter

/** Represents cost models for script languages in the Cardano blockchain.
  *
  * Cost models define the execution costs of operations in different script languages. The keys are
  * language identifiers (0 for Plutus V1, 1 for Plutus V2, 2 for Plutus V3), and the values are
  * lists of integers representing operation costs.
  *
  * @param models
  *   Map from language identifiers to lists of operation costs
  */
case class CostModels(models: Map[Int, IndexedSeq[Long]]) derives Codec {

    private given LanguageViewEncoder: Encoder[CostModels] with
        def write(w: Writer, costModels: CostModels): Writer = {
            val size = costModels.models.size
            w.writeMapHeader(size)
            for (langId, costModel) <- costModels.models.toArray.sortWith(_._1 < _._1) do
                langId match
                    case 0 =>
                        // For PlutusV1 (language id 0), the language view is the following:
                        //   * the value of costmdls map at key 0 is encoded as an indefinite length
                        //     list and the result is encoded as a bytestring. (our apologies)
                        //   * the language ID tag is also encoded twice. first as a uint then as
                        //     a bytestring.
                        // PlutusV1: Double-bagged encoding for cost model as well
                        // here we must use indefinite CBOR map encoding for backward compatibility
                        val encodedModel = Cbor.encode(costModel.toList).toByteArray
                        w.writeBytes(Array(0.toByte))
                        w.writeBytes(encodedModel)
                    case 1 | 2 => // PlutusV2 - uses standard encoding
                        w.writeInt(langId)
                        w.write(costModel)
                    case _ =>
                        throw new IllegalArgumentException(s"Unknown language ID: $langId")
            end for
            w
        }

    /** Generate language view encoding for script integrity hash computation.
      *
      * This implements the same logic as Cardano's getLanguageView function. For PlutusV1, it uses
      * double-bagged encoding for backward compatibility. For PlutusV2/V3, it uses standard
      * encoding.
      */
    def getLanguageViewEncoding: Array[Byte] = {
        Cbor.encode(this)(using LanguageViewEncoder).toByteArray
    }
}

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

opaque type OriginalCborByteArray <: Array[Byte] = Array[Byte]
object OriginalCborByteArray {

    /** Create an OriginalCborByteArray from a byte array */
    def apply(bytes: Array[Byte]): OriginalCborByteArray = bytes
}

case class KeepRaw[A](value: A, raw: Array[Byte]) {
    override def hashCode: Int =
        util.Arrays.hashCode(Array(value.hashCode(), util.Arrays.hashCode(raw)))

    override def equals(obj: Any): Boolean = obj.asMatchable match {
        case that: KeepRaw[?] =>
            this.value == that.value && java.util.Arrays.equals(this.raw, that.raw)
        case _ => false
    }
    override def toString: String = s"KeepRaw(value=$value, raw=${raw.toHex})"
}

object KeepRaw {
    def apply[A: Encoder](value: A): KeepRaw[A] = new KeepRaw(value, Cbor.encode(value).toByteArray)

    given [A: Decoder](using OriginalCborByteArray): Decoder[KeepRaw[A]] =
        Decoder { r =>
            // Here we need to call `dataItem()` to ensure the cursor is updated
            // see https://github.com/sirthias/borer/issues/761#issuecomment-2919035884
            r.dataItem()
            val start = r.cursor
            val value = r.read[A]()
            val di = r.dataItem()
            val end = if di == DataItem.EndOfInput then r.input.cursor else r.cursor
            val raw = summon[OriginalCborByteArray].slice(start.toInt, end.toInt)
            KeepRaw(value, raw)
        }

    given [A: Encoder]: Encoder[KeepRaw[A]] = (w: Writer, value: KeepRaw[A]) => {
        // FIXME: use w.writeValueAsRawBytes instead of re-encoding when it's supported:
        // https://github.com/sirthias/borer/issues/764
        summon[Encoder[A]].write(w, value.value)
    }
}

extension (self: KeepRaw[Data]) {
    def dataHash: DataHash = {
        // We need to calculate the hash of the raw bytes, not the decoded data
        Hash(platform.blake2b_256(ByteString.unsafeFromArray(self.raw)))
    }
}

case class Sized[A](value: A, size: Int) {
    override def hashCode: Int =
        util.Arrays.hashCode(Array(value.hashCode(), size))

    override def equals(obj: Any): Boolean = obj.asMatchable match {
        case that: Sized[?] =>
            this.value == that.value && this.size == that.size
        case _ => false
    }
    override def toString: String = s"Sized(value=$value, size=$size)"
}

object Sized {
    def apply[A: Encoder](value: A): Sized[A] =
        new Sized(value, Cbor.encode(value).toByteArray.length)

    given [A: Decoder](using OriginalCborByteArray): Decoder[Sized[A]] =
        Decoder { r =>
            // Here we need to call `dataItem()` to ensure the cursor is updated
            // see https://github.com/sirthias/borer/issues/761#issuecomment-2919035884
            r.dataItem()
            val start = r.cursor
            val value = r.read[A]()
            val di = r.dataItem()
            val end = if di == DataItem.EndOfInput then r.input.cursor else r.cursor
            Sized(value, end.toInt - start.toInt)
        }

    given [A: Encoder]: Encoder[Sized[A]] = (w: Writer, value: Sized[A]) => {
        // FIXME: use w.writeValueAsRawBytes instead of re-encoding when it's supported:
        // https://github.com/sirthias/borer/issues/764
        summon[Encoder[A]].write(w, value.value)
    }
}
