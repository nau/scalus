package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.utils.{AllNeededScriptHashesHelper, AllReferenceScriptsHelper, AllWitnessScriptsHelper}

// It's babbageMissingScripts in cardano-ledger
object MissingScriptsValidator extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = {
        for
            neededScriptHashes <- AllNeededScriptHashesHelper.allNeededScriptHashes(
              state.utxo,
              event
            )
            referenceScriptHashes <- AllReferenceScriptsHelper.allReferenceScriptHashes(
              state.utxo,
              event
            )
            witnessScriptHashes = AllWitnessScriptsHelper.allWitnessScriptHashes(event)
            neededScriptHashesNonRefs = neededScriptHashes.diff(referenceScriptHashes)
            _ <-
                val missing = neededScriptHashesNonRefs.diff(witnessScriptHashes)
                if missing.isEmpty then success
                else
                    failure(
                      IllegalArgumentException(
                        s"Missing scripts: $missing transactionId ${event.id}"
                      )
                    )
            _ <-
                val extra = witnessScriptHashes.diff(neededScriptHashesNonRefs)
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
