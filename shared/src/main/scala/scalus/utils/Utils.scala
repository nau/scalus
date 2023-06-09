package scalus.utils

import io.bullet.borer.Codec
import scalus.uplc.Program

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.nio.file.*
import io.bullet.borer.Json
import io.bullet.borer.Cbor
import scalus.uplc.ProgramFlatCodec
import scalus.uplc.DeBruijn

case class PlutusTextEnvelope(`type`: String, description: String, cborHex: String)
object PlutusTextEnvelope {
  import io.bullet.borer.derivation.MapBasedCodecs._
  given Codec[PlutusTextEnvelope] = deriveCodec[PlutusTextEnvelope]
}

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

  def writePlutusFile(path: String, program: Program): Unit =
    val content = programToPlutusFileContent(program)
    Files.write(Paths.get(path), content)

  def programToPlutusFileContent(program: Program): Array[Byte] =
    val `type` = program.version match
      case (1, 0, 0) => "PlutusScriptV1"
      case (2, 0, 0) => "PlutusScriptV2"
      case _         => throw new Exception(s"Unsupported Plutus version: ${program.version}")
    Json.encode(PlutusTextEnvelope(`type`, "", program.doubleCborHex)).toByteArray

  def readPlutusFileContent(content: Array[Byte]): Program =
    val envelope = Json.decode(content).to[PlutusTextEnvelope].value
    val version = envelope.`type` match
      case "PlutusScriptV1" => (1, 0, 0)
      case "PlutusScriptV2" => (2, 0, 0)
      case _ => throw new Exception(s"Unsupported Plutus version: ${envelope.`type`}")
    val doubleCborHex = envelope.cborHex
    val cbor = Cbor.decode(Utils.hexToBytes(doubleCborHex)).to[Array[Byte]].value
    val scriptFlat = Cbor.decode(cbor).to[Array[Byte]].value
    val debruijnedProgram = ProgramFlatCodec.decodeFlat(scriptFlat)
    val program = DeBruijn.fromDeBruijnProgram(debruijnedProgram)
    if program.version != version then
      throw new Exception(s"Version mismatch: ${program.version} != ${version}")
    program

  def readPlutusFile(path: String): Program =
    val content = Files.readAllBytes(Paths.get(path))
    readPlutusFileContent(content)
