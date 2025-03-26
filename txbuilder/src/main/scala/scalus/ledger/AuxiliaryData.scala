package scalus.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import scalus.builtin.ByteString
import scalus.ledger.api.Timelock

import scala.collection.immutable

/** Metadata is a map from metadatum labels to metadatum values */
type Metadata = Map[TransactionMetadatumLabel, TransactionMetadatum]

/** Represents a transaction metadatum label in Cardano */
case class TransactionMetadatumLabel(value: Long) derives Codec:
    require(value >= 0, s"Metadatum label must be non-negative, got $value")

/** Represents transaction metadata in Cardano */
enum TransactionMetadatum:
    /** Map metadata */
    case Map(entries: immutable.Map[TransactionMetadatum, TransactionMetadatum])

    /** List metadata */
    case List(items: Seq[TransactionMetadatum])

    /** Integer metadata */
    case Int(value: Long)

    /** Bytes metadata (max 64 bytes) */
    case Bytes(value: ByteString)

    /** Text metadata (max 64 chars) */
    case Text(value: String)

object TransactionMetadatum:
    /** Maximum allowed size for bytes and text */
    private val MaxSize = 64

    /** CBOR encoder for TransactionMetadatum */
    given Encoder[TransactionMetadatum] with
        def write(w: Writer, value: TransactionMetadatum): Writer = value match
            case TransactionMetadatum.Map(entries) =>
                w.writeMapHeader(entries.size)
                entries.foreach { case (k, v) =>
                    write(w, k)
                    write(w, v)
                }
                w

            case TransactionMetadatum.List(items) =>
                w.writeArrayHeader(items.size)
                items.foreach(item => write(w, item))
                w

            case TransactionMetadatum.Int(value) =>
                w.writeLong(value)

            case TransactionMetadatum.Bytes(value) =>
                if value.size > MaxSize then
                    throw new IllegalArgumentException(
                      s"Bytes size exceeds maximum ($MaxSize), got ${value.size}"
                    )
                w.writeBytes(value.bytes)

            case TransactionMetadatum.Text(value) =>
                if value.length > MaxSize then
                    throw new IllegalArgumentException(
                      s"Text length exceeds maximum ($MaxSize), got ${value.length}"
                    )
                w.writeString(value)

    /** CBOR decoder for TransactionMetadatum */
    given Decoder[TransactionMetadatum] with
        def read(r: Reader): TransactionMetadatum =
            import io.bullet.borer.DataItem as DI

            r.dataItem() match
                case DI.MapHeader | DI.MapStart =>
                    val entries =
                        r.read[immutable.Map[TransactionMetadatum, TransactionMetadatum]]()
                    TransactionMetadatum.Map(entries)

                case DI.ArrayHeader | DI.ArrayStart =>
                    val items = r.read[Seq[TransactionMetadatum]]()
                    TransactionMetadatum.List(items)

                case DI.Int | DI.Long | DI.OverLong =>
                    TransactionMetadatum.Int(r.readLong())

                case DI.Bytes | DI.BytesStart =>
                    val bytes = r.read[ByteString]()
                    if bytes.size > MaxSize then
                        r.validationFailure(
                          s"Bytes size exceeds maximum ($MaxSize), got ${bytes.size}"
                        )
                    TransactionMetadatum.Bytes(bytes)

                case DI.Text | DI.TextStart =>
                    val text = r.readString()
                    if text.length > MaxSize then
                        r.validationFailure(
                          s"Text length exceeds maximum ($MaxSize), got ${text.length}"
                        )
                    TransactionMetadatum.Text(text)

                case other =>
                    r.validationFailure(s"Unexpected data item for TransactionMetadatum: $other")

/** Represents auxiliary data in a Cardano transaction */
enum AuxiliaryData:
    /** Shelley-era metadata */
    case Metadata(metadata: Map[TransactionMetadatumLabel, TransactionMetadatum])

    /** Shelley-MA era combined metadata and scripts */
    case MetadataWithScripts(
        metadata: Map[TransactionMetadatumLabel, TransactionMetadatum],
        scripts: List[Timelock]
    )

    /** Alonzo-era and later metadata format with optional components */
    case AlonzoFormat(
        metadata: Option[Map[TransactionMetadatumLabel, TransactionMetadatum]] = None,
        nativeScripts: Option[List[Timelock]] = None,
        plutusV1Scripts: Option[List[ByteString]] = None,
        plutusV2Scripts: Option[List[ByteString]] = None,
        plutusV3Scripts: Option[List[ByteString]] = None
    )

object AuxiliaryData:
    /** CBOR encoder for AuxiliaryData */
    given Encoder[AuxiliaryData] with
        def write(w: Writer, value: AuxiliaryData): Writer = value match
            case AuxiliaryData.Metadata(metadata) =>
                // Metadata is encoded directly
                w.write(metadata)

            case AuxiliaryData.MetadataWithScripts(metadata, scripts) =>
                // Array of [metadata, scripts]
                w.writeArrayHeader(2)
                w.write(metadata)

                // Write scripts array
                w.writeArrayHeader(scripts.size)
                scripts.foreach(script => w.write(script))
                w

            case AuxiliaryData.AlonzoFormat(
                  metadata,
                  nativeScripts,
                  plutusV1Scripts,
                  plutusV2Scripts,
                  plutusV3Scripts
                ) =>
                // Calculate map size
                var mapSize = 0
                if metadata.isDefined then mapSize += 1
                if nativeScripts.isDefined then mapSize += 1
                if plutusV1Scripts.isDefined then mapSize += 1
                if plutusV2Scripts.isDefined then mapSize += 1
                if plutusV3Scripts.isDefined then mapSize += 1

                // Write the tag 259 first
                w.writeTag(Tag.Other(259))

                // Then write the map with all fields
                w.writeMapHeader(mapSize)

                // Metadata (key 0)
                metadata.foreach { md =>
                    w.writeInt(0)
                    w.write(md)
                }

                // Native scripts (key 1)
                nativeScripts.foreach { scripts =>
                    w.writeInt(1)
                    w.writeArrayHeader(scripts.size)
                    scripts.foreach(script => w.write(script))
                }

                // Plutus V1 scripts (key 2)
                plutusV1Scripts.foreach { scripts =>
                    w.writeInt(2)
                    w.writeArrayHeader(scripts.size)
                    scripts.foreach(script => w.writeBytes(script.bytes))
                }

                // Plutus V2 scripts (key 3)
                plutusV2Scripts.foreach { scripts =>
                    w.writeInt(3)
                    w.writeArrayHeader(scripts.size)
                    scripts.foreach(script => w.writeBytes(script.bytes))
                }

                // Plutus V3 scripts (key 4)
                plutusV3Scripts.foreach { scripts =>
                    w.writeInt(4)
                    w.writeArrayHeader(scripts.size)
                    scripts.foreach(script => w.writeBytes(script.bytes))
                }

                w

    /** CBOR decoder for AuxiliaryData */
    given Decoder[AuxiliaryData] with
        def read(r: Reader): AuxiliaryData =
            import io.bullet.borer.DataItem as DI

            r.dataItem() match
                case DI.Tag if r.tryReadTag(Tag.Other(259)) =>
                    // Alonzo format with tag
                    // We've already consumed the tag, now read the map
                    val size = r.readMapHeader()
                    var metadata: Option[Map[TransactionMetadatumLabel, TransactionMetadatum]] =
                        None
                    var nativeScripts: Option[List[Timelock]] = None
                    var plutusV1Scripts: Option[List[ByteString]] = None
                    var plutusV2Scripts: Option[List[ByteString]] = None
                    var plutusV3Scripts: Option[List[ByteString]] = None

                    for _ <- 0L until size do
                        val key = r.readInt()
                        key match
                            case 0 => // Metadata
                                metadata = Some(r.read[scalus.ledger.Metadata]())

                            case 1 => // Native scripts
                                nativeScripts = Some(r.read[List[Timelock]]())

                            case 2 => // Plutus V1 scripts
                                plutusV1Scripts = Some(r.read[List[ByteString]]())

                            case 3 => // Plutus V2 scripts
                                plutusV2Scripts = Some(r.read[List[ByteString]]())

                            case 4 => // Plutus V3 scripts
                                plutusV3Scripts = Some(r.read[List[ByteString]]())

                            case _ => r.skipDataItem() // Skip unknown fields

                    AuxiliaryData.AlonzoFormat(
                      metadata,
                      nativeScripts,
                      plutusV1Scripts,
                      plutusV2Scripts,
                      plutusV3Scripts
                    )

                case DI.MapHeader | DI.MapStart =>
                    // Simple metadata format
                    AuxiliaryData.Metadata(r.read[scalus.ledger.Metadata]())

                case DI.ArrayHeader | DI.ArrayStart =>
                    // MetadataWithScripts format
                    val size = r.readArrayHeader()
                    if size != 2 then
                        r.validationFailure(
                          s"Expected 2 elements for MetadataWithScripts, got $size"
                        )

                    val metadata = r.read[scalus.ledger.Metadata]()

                    val scripts = r.read[List[Timelock]]()
                    AuxiliaryData.MetadataWithScripts(metadata, scripts)

                case di =>
                    r.validationFailure(
                      s"Expected Map, MapStart, Array, or ArrayStart for AuxiliaryData, got ${DataItem
                              .stringify(di)}"
                    )
