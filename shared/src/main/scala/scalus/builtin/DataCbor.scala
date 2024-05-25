package scalus.builtin
import io.bullet.borer.Tag
import io.bullet.borer.Tag.{NegativeBigNum, Other, PositiveBigNum}
import io.bullet.borer.{ByteAccess, DataItem as DI, Decoder, Encoder, Reader, Writer}
import scalus.builtin.Data.{B, Constr, I, Map}

import java.math.BigInteger
import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/** CBOR encoder for the [[Data]] type. The encoding and decoding logic is based on the
  * [[https://github.com/IntersectMBO/plutus/blob/441b76d9e9745dfedb2afc29920498bdf632f162/plutus-core/plutus-core/src/PlutusCore/Data.hs#L72 Cardano node implementation]].
  */
given Encoder[Data] with
    override def write(writer: Writer, data: Data): Writer =
        given Encoder[Data] = this

        def writeChunkedByteArray(value: Array[Byte]) = {
            if value.length <= 64 then writer.writeBytes(value)
            else writer.writeBytesIterator(value.grouped(64))
        }

        def writeChunkedBigInt(x: BigInt) = {
            // use the default BigInt serialization, it works same as in the Cardano node
            if x.bitLength <= 64 then writer.write(x)
            else
                // otherwise, chunk the bytes
                val bytes = x.toByteArray
                if x.signum < 0 then
                    bytes.mapInPlace(b => (~b.toInt).toByte) // basically, -1 - x
                    writer.writeTag(Tag.NegativeBigNum)
                else writer.writeTag(Tag.PositiveBigNum)
                writeChunkedByteArray(bytes)
        }

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
            case I(value)          => writeChunkedBigInt(value)
            case B(value)          => writeChunkedByteArray(value.bytes)
    /*
     * Cardano stores maps as a list of key-value pairs
     * Note, that it allows duplicate keys!
     * This is why we don't use the `writeMap` method from `Writer` here
     */
    private def writeMap[A: Encoder, B: Encoder](writer: Writer, x: Iterable[(A, B)]): Writer =
        if x.nonEmpty then
            val iterator = x.iterator
            writer.writeMapHeader(x.size)
            while (iterator.hasNext)
                val (k, v) = iterator.next()
                writer.write(k)
                writer.write(v)
        else writer.writeEmptyMap()
        writer

/** CBOR decoder for the [[Data]] type. The encoding and decoding logic is based on the
  * [[https://github.com/IntersectMBO/plutus/blob/441b76d9e9745dfedb2afc29920498bdf632f162/plutus-core/plutus-core/src/PlutusCore/Data.hs#L72 Cardano node implementation]].
  */
given Decoder[Data] with

    override def read(r: Reader): Data =
        given Decoder[Data] = this

        val maxCborByteArraySize = 64

        // read sized bytes bounded by 64 bytes
        def readBoundedSizedBytes(): Array[Byte] =
            val bytes = r.readSizedBytes()(using ByteAccess.ForByteArray)
            if bytes.length > maxCborByteArraySize then
                r.overflow(s"Bytes chunk ${bytes.length} must be <= 64 bytes")
            bytes

        // read chunks of 64 bytes
        def readBoundedBytesIndef(): Array[Byte] =
            val acc = new ArrayBuffer[Byte](64)
            while !r.tryReadBreak() do acc ++= readBoundedSizedBytes()
            acc.toArray

        // read bytes (sized or indefinite) where chunks are bounded by 64 bytes
        def readBoundedBytes(): Array[Byte] =
            r.dataItem() match
                case DI.Bytes => readBoundedSizedBytes()
                case DI.BytesStart =>
                    r.readBytesStart()
                    readBoundedBytesIndef()
                case _ => r.unexpectedDataItem(expected = "Bytes or BytesStart")

        r.dataItem() match
            case DI.Int | DI.Long | DI.OverLong => I(Decoder.forBigInt.read(r))
            case DI.MapHeader | DI.MapStart     => Map(readMap.read(r))
            case DI.ArrayStart | DI.ArrayHeader => Data.List(Decoder.forArray[Data].read(r).toList)
            case DI.Bytes | DI.BytesStart       => B(ByteString.unsafeFromArray(readBoundedBytes()))
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
                    case PositiveBigNum => I(BigInteger(1, readBoundedBytes()))
                    case NegativeBigNum => I(BigInteger(1, readBoundedBytes()).not)
                    case _ => r.unexpectedDataItem("Allowed Data Constr Tag or CBOR BigNum Tag")
            case i => r.unexpectedDataItem(s"Allowed Data Item")

    /*
     * Cardano stores maps as a list of key-value pairs
     * Note, that it allows duplicate keys!
     * This is why we don't use the `readMap` method from `Reader` here
     */
    private def readMap[A: Decoder, B: Decoder]: Decoder[List[(A, B)]] =
        Decoder { r =>
            if (r.hasMapHeader)
                @tailrec def rec(remaining: Int, map: ListBuffer[(A, B)]): ListBuffer[(A, B)] =
                    if (remaining > 0) rec(remaining - 1, map.append((r[A], r[B])))
                    else map

                val size = r.readMapHeader()
                if (size <= Int.MaxValue) rec(size.toInt, ListBuffer.empty).toList
                else r.overflow(s"Cannot deserialize Map with size $size (> Int.MaxValue)")
            else if r.hasMapStart then
                r.readMapStart()
                val map = ListBuffer.empty[(A, B)]
                while !r.tryReadBreak() do map.append((r[A], r[B]))
                map.toList
            else r.unexpectedDataItem(expected = "Map")
        }
