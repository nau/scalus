package scalus.cardano.ledger

import scalus.cardano.address.Address
import io.bullet.borer.{Decoder, Encoder, Reader, Writer}
import monocle.Lens

/** Represents a transaction output in Cardano. Both Shelley-era and Babbage-era output formats are
  * supported.
  */
sealed trait TransactionOutput:
    def address: Address
    def value: Value
    def scriptRef: Option[ScriptRef]

object TransactionOutput:
    // Note: I think this will erase the distinction between Shelley and Babbage on using `Set`.
    // I don't think the compiler knows that this takes Shelley -> Shelley and Babbage -> Babbage.
    // Is there a better way?
    val valueLens: Lens[TransactionOutput, Value] = {
        val get: TransactionOutput => Value = _.value
        val set: Value => TransactionOutput => TransactionOutput =
            v => {
                case s: Shelley => s.copy(value = v)
                case b: Babbage => b.copy(value = v)
            }
        Lens(get)(set)
    }

    /** Shelley-era transaction output format */
    final case class Shelley(
        override val address: Address,
        override val value: Value,
        datumHash: Option[DataHash] = None
    ) extends TransactionOutput:
        override def scriptRef: Option[ScriptRef] = None

    /** Babbage-era transaction output format with extended features */
    final case class Babbage(
        override val address: Address,
        override val value: Value,
        datumOption: Option[DatumOption] = None,
        override val scriptRef: Option[ScriptRef] = None
    ) extends TransactionOutput

    /** Creates a Shelley-era transaction output with the specified address and value.
      *
      * @param address
      *   the destination address for this output
      * @param value
      *   the value (ADA and native tokens) contained in this output
      * @return
      *   a new Shelley transaction output with no datum hash
      */
    def apply(address: Address, value: Value): TransactionOutput = Shelley(address, value, None)

    /** Creates a Shelley-era transaction output with the specified address, value, and optional
      * datum hash.
      *
      * @param address
      *   the destination address for this output
      * @param value
      *   the value (ADA and native tokens) contained in this output
      * @param datumHash
      *   datum hash associated with this output
      * @return
      *   a new Shelley transaction output
      */
    def apply(
        address: Address,
        value: Value,
        datumHash: DataHash
    ): TransactionOutput = Shelley(address, value, Some(datumHash))

    /** Creates a Babbage-era transaction output with the specified address, value, and optional
      * datum.
      *
      * @param address
      *   the destination address for this output
      * @param value
      *   the value (ADA and native tokens) contained in this output
      * @param datumOption
      *   optional datum associated with this output
      * @return
      *   a new Babbage transaction output with no script reference
      */
    def apply(
        address: Address,
        value: Value,
        datumOption: Option[DatumOption]
    ): TransactionOutput = Babbage(address, value, datumOption, None)

    /** Creates a Babbage-era transaction output with the specified address, value, and optional
      * datum.
      *
      * @param address
      *   the destination address for this output
      * @param value
      *   the value (ADA and native tokens) contained in this output
      * @param datumOption
      *   datum associated with this output
      * @return
      *   a new Babbage transaction output with no script reference
      */
    def apply(
        address: Address,
        value: Value,
        datumOption: DatumOption
    ): TransactionOutput = Babbage(address, value, Some(datumOption), None)

    /** Creates a Babbage-era transaction output with all optional parameters.
      *
      * @param address
      *   the destination address for this output
      * @param value
      *   the value (ADA and native tokens) contained in this output
      * @param datumOption
      *   optional datum associated with this output
      * @param scriptRef
      *   optional script reference
      * @return
      *   a new Babbage transaction output
      */
    def apply(
        address: Address,
        value: Value,
        datumOption: Option[DatumOption],
        scriptRef: Option[ScriptRef]
    ): TransactionOutput = Babbage(address, value, datumOption, scriptRef)

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

        val address = r.read[Address]()
        val value = r.read[Value]()

        val datumHash =
            if size == 3 then Some(r.read[DataHash]())
            else None

        TransactionOutput.Shelley(address, value, datumHash)

    /** Helper method to read Babbage-era output from CBOR */
    private def readBabbageOutput(r: Reader): TransactionOutput.Babbage =
        val size = r.readMapHeader()

        var address: Option[Address] = None
        var value: Option[Value] = None
        var datumOption: Option[DatumOption] = None
        var scriptRef: Option[ScriptRef] = None

        for _ <- 0L until size do
            r.readInt() match
                case 0     => address = Some(r.read[Address]())
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
