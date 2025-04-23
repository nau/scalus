package scalus.utils

import scala.util.control.NonFatal

object Hex:
    private val HEX_ARRAY = "0123456789abcdef".toCharArray

    extension (bytes: Array[Byte]) def toHex: String = bytesToHex(bytes)

    def bytesToHex(bytes: Array[Byte]): String =
        val hexChars = new Array[Char](bytes.length * 2)
        var j = 0
        while j < bytes.length do
            val v = bytes(j) & 0xff
            hexChars(j * 2) = HEX_ARRAY(v >>> 4)
            hexChars(j * 2 + 1) = HEX_ARRAY(v & 0x0f)
            j += 1
        new String(hexChars)

    def hexToBytes(hex: String): Array[Byte] =
        val hexString = hex.replaceAll("\\s+", "")
        try
            if (hexString.length & 1) != 0 then sys.error("string length is not even")
            hexString.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
        catch
            case NonFatal(e) =>
                throw new IllegalArgumentException(s"`$hexString` is not a valid hex string", e)
