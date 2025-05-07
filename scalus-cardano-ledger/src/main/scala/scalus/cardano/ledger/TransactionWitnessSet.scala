package scalus.cardano.ledger

import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.builtin.given
import scalus.ledger.api.Timelock
import io.bullet.borer.{DataItem, Decoder, Encoder, Reader, Tag, Writer}

given Encoder[ByteString] with
    def write(w: Writer, value: ByteString): Writer =
        w.writeBytes(value.bytes)

given Decoder[ByteString] with
    def read(r: Reader): ByteString =
        ByteString.unsafeFromArray(r.readBytes())

/** Represents the witness set for a transaction in Cardano */
case class TransactionWitnessSet(
    /** VKey witnesses */
    vkeyWitnesses: Option[Set[VKeyWitness]] = None,

    /** Native scripts */
    nativeScripts: Option[Set[Timelock]] = None,

    /** Bootstrap witnesses (for Byron addresses) */
    bootstrapWitnesses: Option[Set[BootstrapWitness]] = None,

    /** Plutus V1 scripts */
    plutusV1Scripts: Option[Set[ByteString]] = None,

    /** Plutus data values */
    plutusData: Option[Set[Data]] = None,

    /** Redeemers */
    redeemers: Option[Redeemers] = None,

    /** Plutus V2 scripts */
    plutusV2Scripts: Option[Set[ByteString]] = None,

    /** Plutus V3 scripts */
    plutusV3Scripts: Option[Set[ByteString]] = None
):
    /** Validate that all sets are non-empty if present */
    require(
      vkeyWitnesses.forall(_.nonEmpty),
      "If vkey witnesses are present, they must be non-empty"
    )
    require(
      nativeScripts.forall(_.nonEmpty),
      "If native scripts are present, they must be non-empty"
    )
    require(
      bootstrapWitnesses.forall(_.nonEmpty),
      "If bootstrap witnesses are present, they must be non-empty"
    )
    require(
      plutusV1Scripts.forall(_.nonEmpty),
      "If Plutus V1 scripts are present, they must be non-empty"
    )
    require(
      plutusData.forall(_.nonEmpty),
      "If Plutus data values are present, they must be non-empty"
    )
    require(
      plutusV2Scripts.forall(_.nonEmpty),
      "If Plutus V2 scripts are present, they must be non-empty"
    )
    require(
      plutusV3Scripts.forall(_.nonEmpty),
      "If Plutus V3 scripts are present, they must be non-empty"
    )

    /** Check if the witness set is empty */
    def isEmpty: Boolean =
        vkeyWitnesses.isEmpty &&
            nativeScripts.isEmpty &&
            bootstrapWitnesses.isEmpty &&
            plutusV1Scripts.isEmpty &&
            plutusData.isEmpty &&
            redeemers.isEmpty &&
            plutusV2Scripts.isEmpty &&
            plutusV3Scripts.isEmpty

object TransactionWitnessSet:
    /** Empty witness set */
    val empty: TransactionWitnessSet = TransactionWitnessSet()

    /** CBOR encoder for TransactionWitnessSet */
    given Encoder[TransactionWitnessSet] with
        def write(w: Writer, value: TransactionWitnessSet): Writer =
            // Count the number of fields to write
            var mapSize = 0

            if value.vkeyWitnesses.isDefined then mapSize += 1
            if value.nativeScripts.isDefined then mapSize += 1
            if value.bootstrapWitnesses.isDefined then mapSize += 1
            if value.plutusV1Scripts.isDefined then mapSize += 1
            if value.plutusData.isDefined then mapSize += 1
            if value.redeemers.isDefined then mapSize += 1
            if value.plutusV2Scripts.isDefined then mapSize += 1
            if value.plutusV3Scripts.isDefined then mapSize += 1

            w.writeMapHeader(mapSize)

            // VKey witnesses (key 0)
            value.vkeyWitnesses.foreach { witnesses =>
                w.writeInt(0)
                writeSet(w, witnesses)
            }

            // Native scripts (key 1)
            value.nativeScripts.foreach { scripts =>
                w.writeInt(1)
                writeSet(w, scripts)
            }

            // Bootstrap witnesses (key 2)
            value.bootstrapWitnesses.foreach { witnesses =>
                w.writeInt(2)
                writeSet(w, witnesses)
            }

            // Plutus V1 scripts (key 3)
            value.plutusV1Scripts.foreach { scripts =>
                w.writeInt(3)
                writeSet(w, scripts)
            }

            // Plutus data (key 4)
            value.plutusData.foreach { data =>
                w.writeInt(4)
                writeSet(w, data)
            }

            // Redeemers (key 5)
            value.redeemers.foreach { redeemers =>
                w.writeInt(5)
                w.write(redeemers)
            }

            // Plutus V2 scripts (key 6)
            value.plutusV2Scripts.foreach { scripts =>
                w.writeInt(6)
                writeSet(w, scripts)
            }

            // Plutus V3 scripts (key 7)
            value.plutusV3Scripts.foreach { scripts =>
                w.writeInt(7)
                writeSet(w, scripts)
            }

            w

    /** Helper to write a Set as CBOR */
    private def writeSet[A](w: Writer, set: Set[A])(using encoder: Encoder[A]): Writer =
        // Use indefinite array
        w.writeTag(Tag.Other(258))
        w.writeArrayHeader(set.size)
        set.foreach(encoder.write(w, _))
        w

    /** CBOR decoder for TransactionWitnessSet */
    given Decoder[TransactionWitnessSet] with
        def read(r: Reader): TransactionWitnessSet =
            val mapSize = r.readMapHeader()

            var vkeyWitnesses: Option[Set[VKeyWitness]] = None
            var nativeScripts: Option[Set[Timelock]] = None
            var bootstrapWitnesses: Option[Set[BootstrapWitness]] = None
            var plutusV1Scripts: Option[Set[ByteString]] = None
            var plutusData: Option[Set[Data]] = None
            var redeemers: Option[Redeemers] = None
            var plutusV2Scripts: Option[Set[ByteString]] = None
            var plutusV3Scripts: Option[Set[ByteString]] = None

            for _ <- 0L until mapSize do
                val key = r.readInt()

                key match
                    case 0 => // VKey witnesses
                        vkeyWitnesses = readSet(r)

                    case 1 => // Native scripts
                        nativeScripts = readSet(r)

                    case 2 => // Bootstrap witnesses
                        bootstrapWitnesses = readSet(r)

                    case 3 => // Plutus V1 scripts
                        plutusV1Scripts = readSet(r)

                    case 4 => // Plutus data
                        plutusData = readSet(r)

                    case 5 => // Redeemers
                        redeemers = Some(r.read[Redeemers]())

                    case 6 => // Plutus V2 scripts
                        plutusV2Scripts = readSet(r)

                    case 7 => // Plutus V3 scripts
                        plutusV3Scripts = readSet(r)

                    case _ => r.skipDataItem() // Skip unknown fields

            plutusData match
                case Some(data) if data.isEmpty =>
                    println("Empty plutus data")
                case _ =>

            TransactionWitnessSet(
              vkeyWitnesses = vkeyWitnesses,
              nativeScripts = nativeScripts,
              bootstrapWitnesses = bootstrapWitnesses,
              plutusV1Scripts = plutusV1Scripts,
              plutusData = plutusData,
              redeemers = redeemers,
              plutusV2Scripts = plutusV2Scripts,
              plutusV3Scripts = plutusV3Scripts
            )

    /** Helper to read a Set from CBOR */
    private def readSet[A](r: Reader)(using decoder: Decoder[A]): Option[Set[A]] =
        // Check for indefinite array tag (258)
        if r.dataItem() == DataItem.Tag then
            val tag = r.readTag()
            if tag.code != 258 then
                r.validationFailure(s"Expected tag 258 for definite Set, got $tag")
            val set = r.read[Set[A]]()
            if set.isEmpty then r.validationFailure("Set must be non-empty")
            Some(set)
        else
            val set = r.read[Set[A]]()
            if set.isEmpty then None else Some(set)
