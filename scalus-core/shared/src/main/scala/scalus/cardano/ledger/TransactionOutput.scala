package scalus.cardano.ledger

import io.bullet.borer.{Decoder, Encoder, Reader, Writer}

/** Represents a transaction output in Cardano. Both Shelley-era and Babbage-era output formats are
  * supported.
  */
sealed trait TransactionOutput:
    def address: AddressBytes
    def value: Value

object TransactionOutput:
    /** Shelley-era transaction output format */
    final case class Shelley(
        override val address: AddressBytes,
        override val value: Value,
        datumHash: Option[DataHash] = None
    ) extends TransactionOutput

    /** Babbage-era transaction output format with extended features */
    final case class Babbage(
        override val address: AddressBytes,
        override val value: Value,
        datumOption: Option[DatumOption] = None,
        scriptRef: Option[ScriptRef] = None
    ) extends TransactionOutput

    /** CBOR encoder for TransactionOutput */
    given Encoder[TransactionOutput] with
        def write(w: Writer, value: TransactionOutput): Writer = value match
            case TransactionOutput.Shelley(address, value, datumHashOpt) =>
                val size = if datumHashOpt.isDefined then 3 else 2
                w.writeArrayHeader(size)

                // Write address
                w.write(address)

                // Write value
                w.write(value)

                // Write optional datum hash
                datumHashOpt.foreach { hash =>
                    w.write(hash)
                }

                w

            case TransactionOutput.Babbage(address, val1, datumOpt, scriptRefOpt) =>
                // Calculate map size based on optional fields
                val size = 2 +
                    (if datumOpt.isDefined then 1 else 0) +
                    (if scriptRefOpt.isDefined then 1 else 0)

                w.writeMapHeader(size)

                // Write address (key 0)
                w.writeInt(0)
                w.write(address)

                // Write value (key 1)
                w.writeInt(1)
                w.write(val1)

                // Write optional datum (key 2)
                datumOpt.foreach { datum =>
                    w.writeInt(2)
                    w.write(datum)
                }

                // Write optional script reference (key 3)
                scriptRefOpt.foreach { script =>
                    w.writeInt(3)
                    w.write(script)
                }

                w

    /** CBOR decoder for TransactionOutput */
    given Decoder[TransactionOutput] with
        def read(r: Reader): TransactionOutput =
            // Check the data item type to determine the format
            if r.hasArrayHeader then readShelleyOutput(r)
            else if r.hasMapHeader then readBabbageOutput(r)
            else r.validationFailure("Expected Array or Map for TransactionOutput")

    /** Helper method to read Shelley-era output from CBOR */
    private def readShelleyOutput(r: Reader): TransactionOutput.Shelley =
        val size = r.readArrayHeader()

        if size < 2 || size > 3 then
            r.validationFailure(s"Expected 2 or 3 elements for ShelleyTransactionOutput, got $size")

        val address = r.read[AddressBytes]()
        val value = r.read[Value]()

        val datumHash =
            if size == 3 then Some(r.read[DataHash]())
            else None

        TransactionOutput.Shelley(address, value, datumHash)

    /** Helper method to read Babbage-era output from CBOR */
    private def readBabbageOutput(r: Reader): TransactionOutput.Babbage =
        val size = r.readMapHeader()

        var address: Option[AddressBytes] = None
        var value: Option[Value] = None
        var datumOption: Option[DatumOption] = None
        var scriptRef: Option[ScriptRef] = None

        for _ <- 0L until size do
            r.readInt() match
                case 0     => address = Some(r.read[AddressBytes]())
                case 1     => value = Some(r.read[Value]())
                case 2     => datumOption = Some(r.read[DatumOption]())
                case 3     => scriptRef = Some(r.read[ScriptRef]())
                case other => r.skipDataItem() // Skip unknown fields

        // Address and value are required
        if address.isEmpty then
            r.validationFailure(
              "Missing required field 'address' (key 0) in BabbageTransactionOutput"
            )

        if value.isEmpty then
            r.validationFailure(
              "Missing required field 'value' (key 1) in BabbageTransactionOutput"
            )

        TransactionOutput.Babbage(
          address.get,
          value.get,
          datumOption,
          scriptRef
        )
