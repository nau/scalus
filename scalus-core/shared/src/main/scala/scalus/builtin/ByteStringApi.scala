package scalus.builtin

import io.bullet.borer.{Decoder, Encoder, Reader, Writer}

trait ByteStringApi {
    self: ByteString.type =>
    given Encoder[ByteString] with
        def write(w: Writer, value: ByteString): Writer =
            w.writeBytes(value.bytes)

    given Decoder[ByteString] with
        def read(r: Reader): ByteString =
            ByteString.unsafeFromArray(r.readBytes())
}
