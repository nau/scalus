package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.rules.utils.*

// It's babbageMissingScripts in cardano-ledger
object MissingScriptsValidator
    extends STS.Validator,
      AllRequiredScriptHashes,
      AllReferenceScripts,
      AllProvidedScriptHashes {
    override def validate(context: Context, state: State, event: Event): Result = {
        for
            requiredScriptHashes <- allRequiredScriptHashes(state, event)
            referenceScriptHashes <- allReferenceScriptHashes(state, event)
            requiredScriptsHashesNonRefs = requiredScriptHashes.diff(referenceScriptHashes)
            providedScriptHashes = allProvidedScriptHashes(event)
            _ <-
                val missing = requiredScriptsHashesNonRefs.diff(providedScriptHashes)
                if missing.isEmpty then success
                else
                    failure(
                      IllegalArgumentException(
                        s"Missing scripts: $missing transactionId ${event.id}"
                      )
                    )
            _ <-
                val extra = providedScriptHashes.diff(requiredScriptsHashesNonRefs)
                if extra.isEmpty then success
                else
                    failure(
                      IllegalArgumentException(
                        s"Extra scripts: $extra transactionId ${event.id}"
                      )
                    )
        yield ()
    }
}
