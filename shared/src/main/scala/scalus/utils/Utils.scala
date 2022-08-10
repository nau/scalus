package scalus.utils

import scala.util.control.NonFatal

object Utils:
  private val HEX_ARRAY = "0123456789ABCDEF".toCharArray

  // First character to lowercase
  def lowerFirst(s: String): String = s.head.toLower + s.tail

  def bytesToHex(bytes: Array[Byte]): String =
    val hexChars = new Array[Char](bytes.length * 2)
    for j <- bytes.indices do
      val v = bytes(j) & 0xff
      hexChars(j * 2) = HEX_ARRAY(v >>> 4)
      hexChars(j * 2 + 1) = HEX_ARRAY(v & 0x0f)
    new String(hexChars)

  implicit class StringInterpolators(val sc: StringContext) extends AnyVal:

    def hex(args: Any*): Array[Byte] =
      val hexString = sc.s(args: _*).replace(" ", "")
      try
        if (hexString.length & 1) != 0 then sys.error("string length is not even")
        hexString.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
      catch
        case NonFatal(e) =>
          throw new IllegalArgumentException(s"`$hexString` is not a valid hex string", e)
