package scalus.builtin

import io.bullet.borer.{Decoder, Encoder, Reader, Writer}
import scalus.utils.Hex

private trait ByteStringOffchainApi {
    self: ByteString.type =>

    /** Creates an empty ByteString
      *
      * Onchain and offchain operation.
      *
      * @note
      *   This field is specially treated by the Scalus compiler plugin, thus it's not required to
      *   be in the @Compile module.
      */
    val empty = new ByteString(Array.empty)

    /** Creates a ByteString from an [[Array[Byte]]
      *
      * Offchain operation, not available onchain.
      *
      * @note
      *   This method is specially treated by the Scalus compiler plugin, thus it's not required to
      *   be in the @Compile module.
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
      * @note
      *   This method is specially treated by the Scalus compiler plugin, thus it's not required to
      *   be in the @Compile module.
      */
    def fromHex(bytes: String): ByteString = new ByteString(Hex.hexToBytes(bytes))

    /** Creates a ByteString from a UTF-8 encoded string
      *
      * Onchain and offchain operation.
      *
      * @param s
      *   the string to convert to a ByteString
      * @note
      *   This method is specially treated by the Scalus compiler plugin, thus it's not required to
      *   be in the @Compile module.
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
          * @note
          *   This method is specially treated by the Scalus compiler plugin, thus it's not required
          *   to be in the @Compile module.
          */
        def hex(args: Any*): ByteString =
            val hexString = sc.s(args*).replace(" ", "")
            fromHex(hexString)

    given Encoder[ByteString] with
        def write(w: Writer, value: ByteString): Writer =
            w.writeBytes(value.bytes)

    given Decoder[ByteString] with
        def read(r: Reader): ByteString =
            ByteString.unsafeFromArray(r.readBytes())
}
