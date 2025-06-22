package scalus.builtin

import scalus.utils.Hex

import scala.compiletime.asMatchable

// TODO: replace Array on IArray
class ByteString private (val bytes: Array[Byte]) {

    /** Gets byte by index
      *
      * Offchain operation, not available onchain. Use [[ByteString.at]] for onchain access.
      */
    def apply(i: Int): Byte = bytes(i)

    override def toString: String = "\"" + toHex + "\""

    override def hashCode: Int = java.util.Arrays.hashCode(bytes)

    /** Compares two ByteStrings for equality
      *
      * Offchain operation, not available onchain. Use `==` for onchain equality checks.
      */
    override def equals(obj: Any): Boolean = obj.asMatchable match {
        case that: ByteString => java.util.Arrays.equals(this.bytes, that.bytes)
        case _                => false
    }

    /** Converts the ByteString to a hexadecimal string representation
      *
      * Offchain operation, not available onchain.
      */
    lazy val toHex: String = Hex.bytesToHex(bytes)

    /** Converts the ByteString to a binary string representation
      *
      * Offchain operation, not available onchain.
      */
    def toBinaryString: String = bytes.view
        .map(b => String.format("%8s", Integer.toBinaryString(b & 0xff)).replace(' ', '0'))
        .mkString("")

    /** Concatenates two ByteStrings and returns a new ByteString
      *
      * Offchain operation, not available onchain.
      */
    def concat(that: ByteString): ByteString = new ByteString(bytes ++ that.bytes)

    /** The length of the ByteString
      *
      * Offchain operation, not available onchain.
      */
    def size: Int = bytes.length

    /** Checks if the ByteString is empty
      *
      * Offchain operation, not available onchain.
      */
    def isEmpty: Boolean = bytes.isEmpty
}

object ByteString extends ByteStringApi {

    /** Creates an empty ByteString
      *
      * Onchain and offchain operation.
      */
    val empty = new ByteString(Array.empty)

    /** Creates a ByteString from an [[Array[Byte]]
      *
      * Offchain operation, not available onchain.
      */
    def fromArray(bytes: Array[Byte]): ByteString = new ByteString(bytes.clone)

    /** Creates a ByteString from a variable number of bytes
      *
      * Offchain operation, not available onchain.
      */
    def apply(bytes: Byte*): ByteString = new ByteString(bytes.toArray)

    /** Creates a ByteString of a given size filled with a specific byte
      *
      * Offchain operation, not available onchain.
      *
      * @param size
      *   the size of the ByteString
      * @param byte
      *   the byte to fill the ByteString with
      */
    def fill(size: Int, byte: Byte): ByteString =
        val result = new Array[Byte](size)
        if byte != 0 then java.util.Arrays.fill(result, byte)
        new ByteString(result)

    /** Creates a ByteString from an array of bytes without copying the array */
    def unsafeFromArray(bytes: Array[Byte]): ByteString = new ByteString(bytes)

    /** Creates a ByteString from a hexadecimal string
      *
      * Onchain and offchain operation.
      *
      * @param bytes
      *   the hexadecimal string to convert to a ByteString
      */
    def fromHex(bytes: String): ByteString = new ByteString(Hex.hexToBytes(bytes))

    /** Creates a ByteString from a UTF-8 encoded string
      *
      * Onchain and offchain operation.
      *
      * @param s
      *   the string to convert to a ByteString
      */
    def fromString(s: String): ByteString = new ByteString(s.getBytes("UTF-8"))

    extension (sc: StringContext)
        /** Hex string interpolator
          *
          * Works on and offchain. Converts a hexadecimal string to a ByteString.
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
