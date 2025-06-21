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
        inline def slice(from: BigInt, len: BigInt): ByteString =
            Builtins.sliceByteString(from, len, bs)
        inline def at(index: BigInt): BigInt = Builtins.indexByteString(bs, index)
        inline def length: BigInt = Builtins.lengthOfByteString(bs)

        /** Concatenates two ByteStrings and returns a new ByteString */
        inline infix def ++(that: ByteString): ByteString =
            Builtins.appendByteString(bs, that)

    given Encoder[ByteString] with
        def write(w: Writer, value: ByteString): Writer =
            w.writeBytes(value.bytes)

    given Decoder[ByteString] with
        def read(r: Reader): ByteString =
            ByteString.unsafeFromArray(r.readBytes())
}
