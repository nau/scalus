package scalus.utils

import scala.util.control.NonFatal
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream

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

  def hexToBytes(hex: String): Array[Byte] =
    val hexString = hex.replace(" ", "")
    try
      if (hexString.length & 1) != 0 then sys.error("string length is not even")
      hexString.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
    catch
      case NonFatal(e) =>
        throw new IllegalArgumentException(s"`$hexString` is not a valid hex string", e)

  def sha2_256(bytes: Array[Byte]): Array[Byte] =
    val digest = java.security.MessageDigest.getInstance("SHA-256")
    digest.update(bytes)
    digest.digest()

  def uplcToFlat(program: String): Array[Byte] =
    import scala.sys.process.*
    val cmd = "uplc convert --of flat"
    val outStream = new ByteArrayOutputStream()
    cmd.#<(new ByteArrayInputStream(program.getBytes("UTF-8"))).#>(outStream).!
    outStream.toByteArray

  def uplcEvaluate(code: String): String =
    import scala.sys.process.*
    val cmd = "uplc evaluate"
    val out = cmd.#<(new ByteArrayInputStream(code.getBytes("UTF-8"))).!!
    out
