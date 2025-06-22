package scalus.builtin

import io.bullet.borer.{Decoder, Encoder, Reader, Writer}

trait ByteStringApi {
    self: ByteString.type =>

    // These methods are available onchain
    // Because we make them inline and those are builtins
    // we don't need to compile this module (so no @Compile on ByteString)
    extension (bs: ByteString)
        inline infix def <(that: ByteString): Boolean = Builtins.lessThanByteString(bs, that)
        inline infix def <=(that: ByteString): Boolean =
            Builtins.lessThanEqualsByteString(bs, that)
        inline infix def >(that: ByteString): Boolean = Builtins.lessThanByteString(that, bs)
        inline infix def >=(that: ByteString): Boolean =
            Builtins.lessThanEqualsByteString(that, bs)

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
            Builtins.sliceByteString(from, len, bs)

        /** Returns the byte at the specified index in the ByteString
          *
          * @throws BuiltinException
          *   if the index is out of bounds (offchain)
          */
        inline def at(index: BigInt): BigInt = Builtins.indexByteString(bs, index)

        /** Returns the length of the ByteString */
        inline def length: BigInt = Builtins.lengthOfByteString(bs)

        /** Concatenates two ByteStrings and returns a new ByteString */
        inline infix def ++(that: ByteString): ByteString =
            Builtins.appendByteString(bs, that)
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
