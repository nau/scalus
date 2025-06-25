package scalus.cardano.ledger

import io.bullet.borer.*
import scalus.builtin.Data
import scalus.ledger.api.Timelock

/** Represents the witness set for a transaction in Cardano */
case class TransactionWitnessSet(
    /** VKey witnesses */
    vkeyWitnesses: Set[VKeyWitness] = Set.empty,

    /** Native scripts */
    nativeScripts: Set[Timelock] = Set.empty,

    /** Bootstrap witnesses (for Byron addresses) */
    bootstrapWitnesses: Set[BootstrapWitness] = Set.empty,

    /** Plutus V1 scripts */
    plutusV1Scripts: Set[PlutusV1Script] = Set.empty,

    /** Plutus data values */
    plutusData: Set[Data] = Set.empty,

    /** Redeemers */
    redeemers: Option[Redeemers] = None,

    /** Plutus V2 scripts */
    plutusV2Scripts: Set[PlutusV2Script] = Set.empty,

    /** Plutus V3 scripts */
    plutusV3Scripts: Set[PlutusV3Script] = Set.empty
):
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

            if value.vkeyWitnesses.nonEmpty then mapSize += 1
            if value.nativeScripts.nonEmpty then mapSize += 1
            if value.bootstrapWitnesses.nonEmpty then mapSize += 1
            if value.plutusV1Scripts.nonEmpty then mapSize += 1
            if value.plutusData.nonEmpty then mapSize += 1
            if value.redeemers.isDefined then mapSize += 1
            if value.plutusV2Scripts.nonEmpty then mapSize += 1
            if value.plutusV3Scripts.nonEmpty then mapSize += 1

            w.writeMapHeader(mapSize)

            // VKey witnesses (key 0)
            if value.vkeyWitnesses.nonEmpty then
                w.writeInt(0)
                writeSet(w, value.vkeyWitnesses)

            // Native scripts (key 1)
            if value.nativeScripts.nonEmpty then
                w.writeInt(1)
                writeSet(w, value.nativeScripts)

            // Bootstrap witnesses (key 2)
            if value.bootstrapWitnesses.nonEmpty then
                w.writeInt(2)
                writeSet(w, value.bootstrapWitnesses)

            // Plutus V1 scripts (key 3)
            if value.plutusV1Scripts.nonEmpty then
                w.writeInt(3)
                writeSet(w, value.plutusV1Scripts)

            // Plutus data (key 4)
            if value.plutusData.nonEmpty then
                w.writeInt(4)
                writeSet(w, value.plutusData)

            // Redeemers (key 5)
            value.redeemers.foreach { redeemers =>
                w.writeInt(5)
                w.write(redeemers)
            }

            // Plutus V2 scripts (key 6)
            if value.plutusV2Scripts.nonEmpty then
                w.writeInt(6)
                writeSet(w, value.plutusV2Scripts)

            // Plutus V3 scripts (key 7)
            if value.plutusV3Scripts.nonEmpty then
                w.writeInt(7)
                writeSet(w, value.plutusV3Scripts)

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

            var vkeyWitnesses = Set.empty[VKeyWitness]
            var nativeScripts = Set.empty[Timelock]
            var bootstrapWitnesses = Set.empty[BootstrapWitness]
            var plutusV1Scripts = Set.empty[PlutusV1Script]
            var plutusData = Set.empty[Data]
            var redeemers: Option[Redeemers] = None
            var plutusV2Scripts = Set.empty[PlutusV2Script]
            var plutusV3Scripts = Set.empty[PlutusV3Script]

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
    private def readSet[A](r: Reader)(using decoder: Decoder[A]): Set[A] =
        // Check for indefinite array tag (258)
        if r.dataItem() == DataItem.Tag then
            val tag = r.readTag()
            if tag.code != 258 then
                r.validationFailure(s"Expected tag 258 for definite Set, got $tag")
            val set = r.read[Set[A]]()
            if set.isEmpty then r.validationFailure("Set must be non-empty")
            set
        else r.read[Set[A]]()
