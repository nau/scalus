package scalus.utils

import scala.util.control.NonFatal

object Hex:
  private val HEX_ARRAY = "0123456789ABCDEF".toCharArray
  def bytesToHex(bytes: Array[Byte]): String =
    val hexChars = new Array[Char](bytes.length * 2)
    for j <- bytes.indices do
      val v = bytes(j) & 0xff
      hexChars(j * 2) = HEX_ARRAY(v >>> 4)
      hexChars(j * 2 + 1) = HEX_ARRAY(v & 0x0f)
    new String(hexChars)

  def hexToBytes(hex: String): Array[Byte] =
    val hexString = hex.replace(" ", "")
    try
      if (hexString.length & 1) != 0 then sys.error("string length is not even")
      hexString.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
    catch
      case NonFatal(e) =>
        throw new IllegalArgumentException(s"`$hexString` is not a valid hex string", e)
