package scalus.uplc
import io.bullet.borer.Tag.{NegativeBigNum, Other, PositiveBigNum}
import io.bullet.borer.encodings.BaseEncoding
import io.bullet.borer.{DataItem as DI, Decoder, Encoder, Reader, Writer}
import scalus.builtins.ByteString
import scalus.uplc.Data
import scalus.uplc.Data.{B, Constr, I, Map}

object PlutusDataCborEncoder extends Encoder[Data]:
  override def write(writer: Writer, data: Data): Writer =
    implicit val selfEncoder: Encoder[Data] = this
    data match
      case Constr(constr, args) if 0 <= constr && constr < 7 =>
        writer.writeTag(Other(121 + constr))
        writer.writeLinearSeq(args)
      case Constr(constr, args) if 7 <= constr && constr < 128 =>
        writer.writeTag(Other(1280 + (constr - 7)))
        writer.writeLinearSeq(args)
      case Constr(constr, args) =>
        writer.writeTag(Other(102))
        writer.writeArrayHeader(2)
        writer.writeLong(constr)
        writer.writeLinearSeq(args)
      case Map(values)       => writer.writeMap(values.toMap)
      case Data.List(values) => writer.writeLinearSeq(values)
      case I(value)          => writer.write(value)
      case B(value)          => writer.write(value.bytes)

object PlutusDataCborDecoder extends Decoder[Data]:

  override def read(r: Reader): Data =
    implicit val selfDecoder: Decoder[Data] = this

    val maxCborByteArraySize = 64
    def fromByteArray() =
      val byteArray = r.readByteArray()
      if byteArray.length > maxCborByteArraySize then
        r.overflow(
          "ByteArray for decoding JBigInteger is longer than the configured max of " + maxCborByteArraySize + " bytes"
        )
      else new java.math.BigInteger(1, byteArray)

    r.dataItem() match
      case DI.Int | DI.Long | DI.OverLong => I(Decoder.forBigInt.read(r))
      case DI.MapHeader                   => Map(Decoder.forMap[Data, Data].read(r).toList)
      case DI.ArrayStart | DI.ArrayHeader => Data.List(Decoder.forArray[Data].read(r).toList)
      case DI.Bytes =>
        B(
          ByteString.unsafeFromArray(Decoder.forByteArray(BaseEncoding.base16).read(r))
        )
      case DI.Tag =>
        r.readTag() match
          case Other(102) =>
            val len = r.readArrayHeader()
            val i = r.readLong()
            val args = Decoder.forArray[Data].read(r)
            Constr(i, args.toList)
          case Other(value) if 121 <= value && value < 128 =>
            Constr(value - 121, Decoder.forArray[Data].read(r).toList)
          case Other(value) if 1280 <= value && value < 1401 =>
            Constr(value - 1280 + 7, Decoder.forArray[Data].read(r).toList)
          case PositiveBigNum => I(fromByteArray())
          case NegativeBigNum => I(fromByteArray().not)
          case _              => sys.error("Unsupported") // TODO proper exception
      case i => sys.error(s"Unsupported data item $i ${DI.stringify(i)}") // TODO proper exception
