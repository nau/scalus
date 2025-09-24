package scalus.cardano.ledger
package utils

import scalus.builtin.ByteString
import scalus.uplc.DeBruijnedProgram

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
        if majorProtocolVersion < language.majorProtocolVersion then return false

        val decoded = DeBruijnedProgram.fromCborWithRemainingBytes(script.bytes)
        decoded match
            case Right((DeBruijnedProgram(_, term), remaining)) =>
                if language != Language.PlutusV1 && language != Language.PlutusV2 && remaining.nonEmpty
                then return false

                val collectedBuiltins = term.collectBuiltins
                val foundBuiltinsIntroducedIn =
                    Builtins.findBuiltinsIntroducedIn(language, majorProtocolVersion)

                collectedBuiltins.subsetOf(foundBuiltinsIntroducedIn)

            case Left(_) => false
    }
}
