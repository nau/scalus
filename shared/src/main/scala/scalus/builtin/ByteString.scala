package scalus.builtin

import scalus.utils.Hex
import scala.annotation.targetName

class ByteString private (val bytes: Array[Byte]) {
    override def toString: String = "\"" + toHex + "\""

    override def hashCode(): Int = java.util.Arrays.hashCode(bytes)

    override def equals(obj: Any): Boolean = obj match {
        case that: ByteString => java.util.Arrays.equals(this.bytes, that.bytes)
        case _                => false
    }

    lazy val toHex: String = Hex.bytesToHex(bytes)

    /** Concatenates two ByteStrings and returns a new ByteString */
    @targetName("concat")
    infix def ++(that: ByteString): ByteString = new ByteString(bytes ++ that.bytes)

    /** The length of the ByteString */
    def size: Int = bytes.length

    /** The length of the ByteString */
    def length: Int = bytes.length

    def isEmpty: Boolean = bytes.isEmpty
}

object ByteString {
    val empty = new ByteString(Array.empty)
    def fromArray(bytes: Array[Byte]): ByteString = new ByteString(bytes.toArray)

    def apply(bytes: Byte*): ByteString = new ByteString(bytes.toArray)

    def unsafeFromArray(bytes: Array[Byte]): ByteString = new ByteString(bytes)
    def fromHex(bytes: String): ByteString = new ByteString(Hex.hexToBytes(bytes))
    def fromString(s: String): ByteString = new ByteString(s.getBytes("UTF-8"))

    implicit class StringInterpolators(val sc: StringContext) extends AnyVal:

        def hex(args: Any*): ByteString =
            val hexString = sc.s(args: _*).replace(" ", "")
            fromHex(hexString)
}

given Ordering[ByteString] with
    def compare(x: ByteString, y: ByteString): Int =
        java.util.Arrays.compareUnsigned(x.bytes, y.bytes)
