package scalus.cardano.ledger.rules

import scalus.cardano.ledger.rules.utils.*

// It's validateFailedBabbageScripts in cardano-ledger
object NativeScriptValidator
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
//            _ <-
//                val missing = requiredScriptsNonRefs.diff(providedScripts)
//                if missing.isEmpty then success
//                else
//                    failure(
//                      IllegalArgumentException(
//                        s"Missing scripts: $missing transactionId ${event.id}"
//                      )
//                    )
//            _ <-
//                val extra = providedScripts.diff(requiredScriptsNonRefs)
//                if extra.isEmpty then success
//                else
//                    failure(
//                      IllegalArgumentException(
//                        s"Extra scripts: $extra transactionId ${event.id}"
//                      )
//                    )
        yield ()
        ???
    }
}
