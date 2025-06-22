package scalus.builtin

import scalus.Compile
import scalus.utils.Hex

import scala.compiletime.asMatchable

// TODO: replace Array on IArray
class ByteString private[builtin] (val bytes: Array[Byte]) {

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

@Compile
object ByteString extends ByteStringOffchainApi {
    // These methods are available onchain
    // Because we make them inline and those are builtins
    // we don't need to compile this module (so no @Compile on ByteString)
    extension (self: ByteString)
        /** Checks if one 'ByteString' is less than another */
        inline infix def <(that: ByteString): Boolean = Builtins.lessThanByteString(self, that)

        /** Checks if one 'ByteString' is less than or equal to another */
        inline infix def <=(that: ByteString): Boolean =
            Builtins.lessThanEqualsByteString(self, that)

        /** Checks if one 'ByteString' is greater than another */
        inline infix def >(that: ByteString): Boolean = Builtins.lessThanByteString(that, self)

        /** Checks if one 'ByteString' is greater than or equal to another */
        inline infix def >=(that: ByteString): Boolean =
            Builtins.lessThanEqualsByteString(that, self)

        /** Returns a new ByteString that is a slice of the original ByteString
          *
          * @param from
          *   the starting index of the slice (inclusive)
          * @param len
          *   the length of the slice
          * @example
          *   {{{
          *   hex"1234567890abcdef".slice(2, 4) // returns hex"567890ab"
          *   hex"1234567890abcdef".slice(5, 4) // returns hex"abcdef"
          *   hex"1234567890abcdef".slice(9, 4) // returns hex""
          *   hex"1234567890abcdef".slice(0, 0) // returns hex""
          *   }}}
          */
        inline def slice(from: BigInt, len: BigInt): ByteString =
            Builtins.sliceByteString(from, len, self)

        /** Takes the first `n` bytes from the ByteString
          *
          * @param n
          *   the number of bytes to take
          * @example
          *   {{{
          *   hex"1234567890abcdef".take(0) // returns hex""
          *   hex"1234567890abcdef".take(4) // returns hex"12345678"
          *   hex"1234567890abcdef".take(20) // returns hex"1234567890abcdef"
          *   hex"".take(20) // returns hex""
          *   }}}
          */
        inline def take(n: BigInt): ByteString = Builtins.sliceByteString(0, n, self)

        /** Drops the first `n` bytes from the ByteString
          *
          * @param n
          *   the number of bytes to drop
          * @example
          *   {{{
          *   hex"1234567890abcdef".drop(0) // returns hex"1234567890abcdef"
          *   hex"1234567890abcdef".drop(4) // returns hex"90abcdef"
          *   hex"1234567890abcdef".drop(20) // returns hex""
          *   hex"".drop(20) // returns hex""
          *   }}}
          */
        def drop(n: BigInt): ByteString = {
            self.slice(n, self.length - n)
        }

        /** Returns the byte at the specified index in the ByteString
          *
          * @throws BuiltinException
          *   if the index is out of bounds (offchain)
          */
        inline def at(index: BigInt): BigInt = Builtins.indexByteString(self, index)

        /** Returns the length of the ByteString */
        inline def length: BigInt = Builtins.lengthOfByteString(self)

        /** Concatenates two ByteStrings and returns a new ByteString */
        inline infix def ++(that: ByteString): ByteString =
            Builtins.appendByteString(self, that)

    extension (b: BigInt)
        /** Prepends a BigInt to a ByteString and returns a new ByteString */
        inline infix def +:(bs: ByteString): ByteString = Builtins.consByteString(b, bs)

}

given Ordering[ByteString] with
    def compare(x: ByteString, y: ByteString): Int =
        java.util.Arrays.compareUnsigned(x.bytes, y.bytes)
