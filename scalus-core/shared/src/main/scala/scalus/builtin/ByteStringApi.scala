package scalus.builtin

import io.bullet.borer.{Decoder, Encoder, Reader, Writer}

private trait ByteStringApi {
    self: ByteString.type =>

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

    given Encoder[ByteString] with
        def write(w: Writer, value: ByteString): Writer =
            w.writeBytes(value.bytes)

    given Decoder[ByteString] with
        def read(r: Reader): ByteString =
            ByteString.unsafeFromArray(r.readBytes())
}
