package scalus.builtins
import io.bullet.borer.Tag.{NegativeBigNum, Other, PositiveBigNum}
import io.bullet.borer.encodings.BaseEncoding
import io.bullet.borer.{ByteAccess, DataItem as DI, Decoder, Encoder, Reader, Writer}
import scalus.uplc.Data.{B, Constr, I, Map}

import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

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
            case Map(values)       => writeMap(writer, values)
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
    /*
     * Cardano stores maps as a list of key-value pairs
     * Note, that it allows duplicate keys!
     * This is why we don't use the `writeMap` method from `Writer` here
     */
    private def writeMap[A: Encoder, B: Encoder](writer: Writer, x: Iterable[(A, B)]): Writer =
        if (x.nonEmpty)
            val iterator = x.iterator
            def writeEntries(): Unit =
                while (iterator.hasNext)
                    val (k, v) = iterator.next()
                    writer.write(k)
                    writer.write(v)
            writer.writeMapHeader(x.size)
            writeEntries()
        else writer.writeEmptyMap()
        writer

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
            case DI.MapHeader                   => Map(readMap.read(r))
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
            case i =>
                sys.error(s"Unsupported data item $i ${DI.stringify(i)}") // TODO proper exception

    /*
     * Cardano stores maps as a list of key-value pairs
     * Note, that it allows duplicate keys!
     * This is why we don't use the `readMap` method from `Reader` here
     */
    def readMap[A: Decoder, B: Decoder]: Decoder[List[(A, B)]] =
        Decoder { r =>
            if (r.hasMapHeader)
                @tailrec def rec(remaining: Int, map: ListBuffer[(A, B)]): ListBuffer[(A, B)] =
                    if (remaining > 0) rec(remaining - 1, map.append((r[A], r[B])))
                    else map

                val size = r.readMapHeader()
                if (size <= Int.MaxValue) rec(size.toInt, ListBuffer.empty).toList
                else r.overflow(s"Cannot deserialize Map with size $size (> Int.MaxValue)")
            else if (r.hasMapStart)
                r.readMapStart()

                @tailrec def rec(map: ListBuffer[(A, B)]): ListBuffer[(A, B)] =
                    if (r.tryReadBreak()) map else rec(map.append((r[A], r[B])))

                rec(ListBuffer.empty).toList
            else r.unexpectedDataItem(expected = "Map")
        }
