package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.utils.{AllNeededScripts, AllResolvedScripts}
import scalus.ledger.api.ValidityInterval

// It's validateFailedBabbageScripts in cardano-ledger
object NativeScriptsValidator extends STS.Validator {
    override final type Error = TransactionException.BadInputsUTxOException |
        TransactionException.BadReferenceInputsUTxOException |
        TransactionException.NativeScriptsException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionId = event.id
        val validityInterval = event.validityInterval
        val utxo = state.utxo

        for
            allNeededScriptHashes <- AllNeededScripts.allNeededScriptHashes(
              event,
              utxo
            )

            allProvidedReferenceNativeScripts <- AllResolvedScripts
                .allProvidedReferenceNativeScripts(
                  event,
                  utxo
                )

            allWitnessesNativeScripts = AllResolvedScripts.allWitnessesNativeScripts(event)

            validatorKeys = extractValidatorKeys(event)

            invalidWitnessesNativeScriptHashes = invalidNativeScriptHashes(
              allWitnessesNativeScripts,
              allNeededScriptHashes,
              validatorKeys,
              validityInterval
            )

            invalidProvidedReferenceNativeScriptHashes = invalidNativeScriptHashes(
              allProvidedReferenceNativeScripts,
              allNeededScriptHashes,
              validatorKeys,
              validityInterval
            )

            _ <-
                if invalidWitnessesNativeScriptHashes.nonEmpty ||
                    invalidProvidedReferenceNativeScriptHashes.nonEmpty
                then
                    failure(
                      TransactionException.NativeScriptsException(
                        transactionId,
                        invalidWitnessesNativeScriptHashes,
                        invalidProvidedReferenceNativeScriptHashes
                      )
                    )
                else success
        yield ()
    }

    private def extractValidatorKeys(
        event: Event
    ): Set[AddrKeyHash] = event.witnessSet.vkeyWitnesses.map(_.vkeyHash)

    private def invalidNativeScriptHashes(
        nativeScripts: Set[Script.Native],
        neededScriptHashes: Set[ScriptHash],
        validatorKeys: Set[AddrKeyHash],
        validityInterval: ValidityInterval
    ): Set[ScriptHash] = {
        for
            nativeScript <- nativeScripts if neededScriptHashes.contains(nativeScript.scriptHash) &&
                !nativeScript.script.evaluate(validatorKeys, validityInterval)
        yield nativeScript.scriptHash
    }
}
