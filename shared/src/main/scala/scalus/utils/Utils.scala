package scalus.utils

import scalus.ledger.api.PlutusLedgerLanguage
import scalus.ledger.api.PlutusLedgerLanguage.*
import scalus.uplc.DeBruijnedProgram
import scalus.uplc.Program
import upickle.default.*

import java.nio.file.*

case class PlutusTextEnvelope(`type`: String, description: String, cborHex: String)
    derives ReadWriter

object Utils:
    export Hex.bytesToHex
    export Hex.hexToBytes
    // First character to lowercase
    def lowerFirst(s: String): String =
        if s == null || s.length == 0 || !s.charAt(0).isUpper then s
        else s.updated(0, s.charAt(0).toLower)

    def sha2_256(bytes: Array[Byte]): Array[Byte] =
        val digest = java.security.MessageDigest.getInstance("SHA-256")
        digest.update(bytes)
        digest.digest()

    @deprecated("Use writePlutusFile with DeBruijnedProgram instead", "0.8.4")
    def writePlutusFile(path: String, program: Program, plutusVersion: PlutusLedgerLanguage): Unit =
        writePlutusFile(path, program.deBruijnedProgram, plutusVersion)

    def writePlutusFile(
        path: String,
        program: DeBruijnedProgram,
        plutusVersion: PlutusLedgerLanguage
    ): Unit =
        val content = programToPlutusFileContent(program, plutusVersion)
        Files.write(Paths.get(path), content.getBytes("UTF-8"))

    @deprecated("Use programToPlutusFileContent with DeBruijnedProgram instead", "0.8.4")
    def programToPlutusFileContent(
        program: Program,
        plutusVersion: PlutusLedgerLanguage
    ): String = programToPlutusFileContent(program.deBruijnedProgram, plutusVersion)

    def programToPlutusFileContent(
        program: DeBruijnedProgram,
        plutusVersion: PlutusLedgerLanguage
    ): String =
        val `type` = plutusVersion match
            case PlutusV1 => "PlutusScriptV1"
            case PlutusV2 => "PlutusScriptV2"
            case PlutusV3 => "PlutusScriptV3"
        write(PlutusTextEnvelope(`type`, "", program.doubleCborHex))

    def readPlutusFileContent(content: String): Program =
        val envelope = read[PlutusTextEnvelope](content)
        // TODO: check that the version is supported, validate builtins etc
        val doubleCborHex = envelope.cborHex
        Program.fromDoubleCborHex(doubleCborHex)

    def readPlutusFile(path: String): Program =
        val content = new String(Files.readAllBytes(Paths.get(path)), "UTF-8")
        readPlutusFileContent(content)
