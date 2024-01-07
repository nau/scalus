package scalus.utils

import io.bullet.borer.Cbor
import io.bullet.borer.Codec
import io.bullet.borer.Json
import scalus.ledger.api.PlutusLedgerLanguage
import scalus.ledger.api.PlutusLedgerLanguage.*
import scalus.uplc.DeBruijn
import scalus.uplc.Program
import scalus.uplc.ProgramFlatCodec

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.nio.file.*

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

    def writePlutusFile(path: String, program: Program, plutusVersion: PlutusLedgerLanguage): Unit =
        val content = programToPlutusFileContent(program, plutusVersion)
        Files.write(Paths.get(path), content)

    def programToPlutusFileContent(
        program: Program,
        plutusVersion: PlutusLedgerLanguage
    ): Array[Byte] =
        val `type` = plutusVersion match
            case PlutusV1 => "PlutusScriptV1"
            case PlutusV2 => "PlutusScriptV2"
            case _        => throw new Exception(s"Unsupported Plutus version: ${plutusVersion}")
        Json.encode(PlutusTextEnvelope(`type`, "", program.doubleCborHex)).toByteArray

    def readPlutusFileContent(content: Array[Byte]): Program =
        val envelope = Json.decode(content).to[PlutusTextEnvelope].value
        // TODO: check that the version is supported, validate builtins etc
        val doubleCborHex = envelope.cborHex
        val cbor = Cbor.decode(Utils.hexToBytes(doubleCborHex)).to[Array[Byte]].value
        val scriptFlat = Cbor.decode(cbor).to[Array[Byte]].value
        val debruijnedProgram = ProgramFlatCodec.decodeFlat(scriptFlat)
        val program = DeBruijn.fromDeBruijnProgram(debruijnedProgram)
        program

    def readPlutusFile(path: String): Program =
        val content = Files.readAllBytes(Paths.get(path))
        readPlutusFileContent(content)
