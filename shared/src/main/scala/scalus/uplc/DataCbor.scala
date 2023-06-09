package scalus.uplc
import io.bullet.borer.Decoder
import io.bullet.borer.Encoder
import io.bullet.borer.Reader
import io.bullet.borer.Tag.NegativeBigNum
import io.bullet.borer.Tag.Other
import io.bullet.borer.Tag.PositiveBigNum
import io.bullet.borer.Writer
import io.bullet.borer.encodings.BaseEncoding
import io.bullet.borer.{DataItem => DI}
import scalus.builtins.ByteString
import scalus.uplc.Data.B
import scalus.uplc.Data.Constr
import scalus.uplc.Data.I
import scalus.uplc.Data.Map
import scala.collection.mutable.ArrayBuffer
import io.bullet.borer.ByteAccess

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
      case B(value) =>
        if value.bytes.length <= 64
        then writer.write(value.bytes)
        else
          def to64ByteChunks(bytes: Array[Byte]): List[Array[Byte]] =
            if bytes.length <= 64 then List(bytes)
            else bytes.take(64) :: to64ByteChunks(bytes.drop(64))
          writer.writeBytesIterator(to64ByteChunks(value.bytes).iterator)

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
      case DI.BytesStart =>
        // read chunks of 64 bytes
        def readChunks(): Array[Byte] =
          val acc = new ArrayBuffer[Byte](64)
          while !r.tryReadBreak() do
            val bytes = r.readBytes()(using ByteAccess.ForByteArray)
            if bytes.length > 64 then r.overflow("ByteString chunk is not 64 bytes")
            acc ++= bytes
          acc.toArray
        r.readBytesStart()
        B(ByteString.unsafeFromArray(readChunks()))
      case DI.Tag =>
        r.readTag() match
          case Other(102) =>
            val _ = r.readArrayHeader()
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
