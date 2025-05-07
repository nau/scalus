package scalus.cardano.ledger

import io.bullet.borer.Tag.EmbeddedCBOR
import scalus.builtin.Data
import scalus.builtin.given
import io.bullet.borer.{Cbor, Decoder, Encoder, Reader, Writer}

/** Represents a datum option in Cardano outputs */
enum DatumOption:
    /** Reference to a datum by its hash */
    case Hash(hash: Hash32)

    /** Inline datum value */
    case Inline(data: Data)

object DatumOption:
    /** CBOR encoder for DatumOption */
    given Encoder[DatumOption] with
        def write(w: Writer, value: DatumOption): Writer =
            w.writeArrayHeader(2)
            value match
                case DatumOption.Hash(hash) =>
                    w.writeInt(0)
                    w.write(hash)

                case DatumOption.Inline(data) =>
                    w.writeInt(1)
                    val dataCbor = Cbor.encode(data).toByteArray
                    w.write(EmbeddedCBOR @@ dataCbor)
            w

    /** CBOR decoder for DatumOption */
    given Decoder[DatumOption] with
        def read(r: Reader): DatumOption =
            val size = r.readArrayHeader()
            if size != 2 then r.validationFailure(s"Expected 2 elements for DatumOption, got $size")

            val tag = r.readInt()
            tag match
                case 0 => DatumOption.Hash(r.read[Hash32]())
                case 1 =>
                    val tag = r.readTag()
                    if tag != EmbeddedCBOR then
                        r.validationFailure(s"Expected tag 24 for Data, got $tag")

                    // Read the embedded CBOR bytes
                    val bytes: Array[Byte] = r.readBytes()

                    // Parse the bytes as a Script
                    val data = Cbor.decode(bytes).to[Data].value
                    DatumOption.Inline(data)
                case other => r.validationFailure(s"Invalid DatumOption tag: $tag")
