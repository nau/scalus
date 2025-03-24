package scalus.ledger

import scalus.builtin.Data
import scalus.builtin.given
import io.bullet.borer.{DataItem, Decoder, Encoder, Reader, Writer}

import scala.collection.immutable

/** Represents the purpose of a redeemer in Cardano
  *
  * 0: spend 1: mint 2: cert 3: reward 4: voting 5: proposing
  */
enum RedeemerTag:
    case Spend, Mint, Cert, Reward, Voting, Proposing

object RedeemerTag:
    /** CBOR encoder for RedeemerTag */
    given Encoder[RedeemerTag] with
        def write(w: Writer, value: RedeemerTag): Writer =
            w.writeInt(value.ordinal)

    /** CBOR decoder for RedeemerTag */
    given Decoder[RedeemerTag] with
        def read(r: Reader): RedeemerTag =
            val ordinal = r.readInt()
            if ordinal < 0 || ordinal > 5 then
                r.validationFailure(s"Invalid RedeemerTag value: $ordinal")
            RedeemerTag.fromOrdinal(ordinal)

/** Represents a single redeemer in Cardano transaction A redeemer contains the execution context
  * for a script
  */
case class Redeemer(
    /** The purpose of the redeemer */
    tag: RedeemerTag,

    /** The index within the purpose group */
    index: Int,

    /** The data passed to the script */
    data: Data,

    /** The execution units allocated to the script */
    exUnits: ExUnits
):
    require(index >= 0, s"Redeemer index must be non-negative, got $index")

/** Represents a collection of redeemers in the transaction witness set */
enum Redeemers:
    /** Array-based representation (legacy format) */
    case Array(redeemers: scala.List[Redeemer])

    /** Map-based representation (new format) Maps (tag, index) pairs to (data, exUnits) pairs
      */
    case Map(redeemers: scala.collection.immutable.Map[(RedeemerTag, Int), (Data, ExUnits)])

    /** Convert to list of Redeemer objects */
    def toList: scala.List[Redeemer] = this match
        case Array(list) => list
        case Map(map) =>
            map.map { case ((tag, index), (data, exUnits)) =>
                Redeemer(tag, index, data, exUnits)
            }.toList

object Redeemer:
    /** CBOR encoder for Redeemer */
    given Encoder[Redeemer] with
        def write(w: Writer, value: Redeemer): Writer =
            w.writeArrayHeader(4)
            w.write(value.tag)
            w.writeInt(value.index)
            w.write(value.data)
            w.write(value.exUnits)
            w

    /** CBOR decoder for Redeemer */
    given Decoder[Redeemer] with
        def read(r: Reader): Redeemer =
            val size = r.readArrayHeader()
            if size != 4 then r.validationFailure(s"Expected 4 elements for Redeemer, got $size")

            val tag = RedeemerTag.given_Decoder_RedeemerTag.read(r)
            val index = r.readInt()

            if index < 0 then
                r.validationFailure(s"Redeemer index must be non-negative, got $index")

            val data = r.read[Data]()
            val exUnits = r.read[ExUnits]()

            Redeemer(tag, index, data, exUnits)

object Redeemers:
    /** CBOR encoder for Redeemers */
    given Encoder[Redeemers] with
        def write(w: Writer, value: Redeemers): Writer = value match
            case Redeemers.Array(redeemers) =>
                // Write as array of redeemers
                w.writeArrayHeader(redeemers.size)
                redeemers.foreach { redeemer =>
                    Redeemer.given_Encoder_Redeemer.write(w, redeemer)
                }
                w

            case Redeemers.Map(redeemers) =>
                // Write as map from keys to data+exunits
                w.writeMapHeader(redeemers.size)
                redeemers.foreach { case ((tag, index), (data, exUnits)) =>
                    // Write key as [tag, index]
                    w.writeArrayHeader(2)
                    RedeemerTag.given_Encoder_RedeemerTag.write(w, tag)
                    w.writeInt(index)

                    // Write value as [data, exUnits]
                    w.writeArrayHeader(2)
                    w.write(data)
                    ExUnits.given_Encoder_ExUnits.write(w, exUnits)
                }
                w

    /** CBOR decoder for Redeemers */
    given Decoder[Redeemers] with
        def read(r: Reader): Redeemers =
            r.dataItem() match
                case DataItem.ArrayHeader | DataItem.ArrayStart =>
                    Redeemers.Array(r.read[scala.List[Redeemer]]())
                case DataItem.MapHeader | DataItem.MapStart =>
                    // Map format
                    val redeemers = r.read[immutable.Map[(RedeemerTag, Int), (Data, ExUnits)]]()
                    Redeemers.Map(redeemers)
                case _ => r.validationFailure("Expected Array or Map for Redeemers")
