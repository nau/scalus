package scalus.builtin

import scalus.utils.Hex

import scala.annotation.targetName
import scala.compiletime.asMatchable

// TODO: replace Array on IArray
class ByteString private (val bytes: Array[Byte]) {
    def apply(i: Int): Byte = bytes(i)

    override def toString: String = "\"" + toHex + "\""

    override def hashCode: Int = java.util.Arrays.hashCode(bytes)

    override def equals(obj: Any): Boolean = obj.asMatchable match {
        case that: ByteString => java.util.Arrays.equals(this.bytes, that.bytes)
        case _                => false
    }

    lazy val toHex: String = Hex.bytesToHex(bytes)

    def toBinaryString: String = bytes.view
        .map(b => String.format("%8s", Integer.toBinaryString(b & 0xff)).replace(' ', '0'))
        .mkString("")

    /** Concatenates two ByteStrings and returns a new ByteString */
    @targetName("concat")
    infix def ++(that: ByteString): ByteString = new ByteString(bytes ++ that.bytes)

    /** The length of the ByteString */
    def size: Int = bytes.length

    /** The length of the ByteString */
    def length: Int = bytes.length

    def isEmpty: Boolean = bytes.isEmpty
}

object ByteString extends ByteStringApi {
    val empty = new ByteString(Array.empty)

    def fromArray(bytes: Array[Byte]): ByteString = new ByteString(bytes.clone)

    def apply(bytes: Byte*): ByteString = new ByteString(bytes.toArray)

    def fill(size: Int, byte: Byte): ByteString =
        val result = new Array[Byte](size)
        if byte != 0 then java.util.Arrays.fill(result, byte)
        new ByteString(result)

    def unsafeFromArray(bytes: Array[Byte]): ByteString = new ByteString(bytes)
    def fromHex(bytes: String): ByteString = new ByteString(Hex.hexToBytes(bytes))
    def fromString(s: String): ByteString = new ByteString(s.getBytes("UTF-8"))

    extension (sc: StringContext)
        /** Hex string interpolator
          *
          * @example
          *   {{{
          * val hexString = hex"deadbeef"
          * val withSpaces = hex"de ad be ef"
          * val upperCase = hex"DEADBEEF"
          *   }}}
          */
        def hex(args: Any*): ByteString =
            val hexString = sc.s(args*).replace(" ", "")
            fromHex(hexString)
}

given Ordering[ByteString] with
    def compare(x: ByteString, y: ByteString): Int =
        java.util.Arrays.compareUnsigned(x.bytes, y.bytes)
