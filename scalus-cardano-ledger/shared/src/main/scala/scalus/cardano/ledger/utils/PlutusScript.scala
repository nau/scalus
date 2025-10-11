package scalus.cardano.ledger
package utils

import scalus.builtin.ByteString
import scalus.uplc.{DeBruijnedProgram, ProgramFlatCodec}

import scala.util.control.NonFatal

object PlutusScript {
    def isWellFormed(
        plutusScript: PlutusScript,
        majorProtocolVersion: MajorProtocolVersion
    ): Boolean = {
        isWellFormed(plutusScript.script, plutusScript.language, majorProtocolVersion)
    }

    def isWellFormed(
        script: ByteString,
        language: Language,
        majorProtocolVersion: MajorProtocolVersion
    ): Boolean = {
        if majorProtocolVersion < language.introducedInVersion then return false

        val ProgramFlatCodec.DecodeResult(DeBruijnedProgram(_, term), remaining) =
            try DeBruijnedProgram.fromCborWithRemainingBytes(script.bytes)
            catch case NonFatal(_) => return false

        if language != Language.PlutusV1 && language != Language.PlutusV2 && remaining.nonEmpty
        then return false

        val collectedBuiltins = term.collectBuiltins
        val foundBuiltinsIntroducedIn =
            Builtins.findBuiltinsIntroducedIn(language, majorProtocolVersion)

        collectedBuiltins.subsetOf(foundBuiltinsIntroducedIn)
    }
}
