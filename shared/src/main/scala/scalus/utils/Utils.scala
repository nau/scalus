package scalus.utils

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream

object Utils:
  export Hex.bytesToHex
  export Hex.hexToBytes
  // First character to lowercase
  def lowerFirst(s: String): String = s.head.toLower + s.tail

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
