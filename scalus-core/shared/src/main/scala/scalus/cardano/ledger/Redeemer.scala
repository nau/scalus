package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import scalus.builtin.Data

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
) derives Codec:
    require(index >= 0, s"Redeemer index must be non-negative, got $index")

/** Represents a collection of redeemers in the transaction witness set */
sealed trait Redeemers:
    /** Convert to list of Redeemer objects */
    def toSeq: Seq[Redeemer] = this match
        case Redeemers.Array(list) => list
        case Redeemers.Map(map) =>
            map.map { case ((tag, index), (data, exUnits)) =>
                Redeemer(tag, index, data, exUnits)
            }.toList

object Redeemers:
    /** Array-based representation (legacy format) */
    final case class Array(redeemers: IndexedSeq[Redeemer]) extends Redeemers:
        require(redeemers.nonEmpty, "Must have at least one redeemer")

    /** Map-based representation (new format) Maps (tag, index) pairs to (data, exUnits) pairs
      */
    final case class Map(redeemers: immutable.Map[(RedeemerTag, Int), (Data, ExUnits)])
        extends Redeemers:
        require(redeemers.nonEmpty, "Must have at least one redeemer")

    /** CBOR encoder for Redeemers */
    given Encoder[Redeemers] with
        def write(w: Writer, value: Redeemers): Writer = value match
            case Redeemers.Array(redeemers) =>
                // Write as array of redeemers
                w.writeIndexedSeq(redeemers)

            case Redeemers.Map(redeemers) =>
                // Write as map from keys to data+exunits
                w.writeMapHeader(redeemers.size)
                redeemers.foreach { case ((tag, index), (data, exUnits)) =>
                    // Write key as [tag, index]
                    w.writeArrayHeader(2)
                    w.write(tag)
                    w.writeInt(index)

                    // Write value as [data, exUnits]
                    w.writeArrayHeader(2)
                    w.write(data)
                    w.write(exUnits)
                }
                w

    /** CBOR decoder for Redeemers */
    given Decoder[Redeemers] with
        def read(r: Reader): Redeemers =
            r.dataItem() match
                case DataItem.ArrayHeader | DataItem.ArrayStart =>
                    Redeemers.Array(r.read[IndexedSeq[Redeemer]]())
                case DataItem.MapHeader | DataItem.MapStart =>
                    // Map format
                    val redeemers = r.read[immutable.Map[(RedeemerTag, Int), (Data, ExUnits)]]()
                    Redeemers.Map(redeemers)
                case _ => r.validationFailure("Expected Array or Map for Redeemers")
