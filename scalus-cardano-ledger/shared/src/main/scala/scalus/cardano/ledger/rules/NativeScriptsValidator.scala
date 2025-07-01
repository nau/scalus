package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.rules.utils.*
import scalus.ledger.api.{Timelock, ValidityInterval}

import scala.util.boundary
import scala.util.boundary.break

// It's validateFailedBabbageScripts in cardano-ledger
object NativeScriptsValidator
    extends STS.Validator,
      AllRequiredScriptHashes,
      AllReferenceScripts,
      AllProvidedScripts {
    override def validate(context: Context, state: State, event: Event): Result = {
        for
            requiredScriptHashes <- allRequiredScriptHashes(state, event)
            referenceNativeScripts <- allReferenceNativeScripts(state, event)
            providedNativeScripts = allProvidedNativeScripts(event)
            validatorKeys = allValidatorKeys(event)
            validityInterval = extractValidityInterval(event)
            _ <- validateNativeScripts(
              referenceNativeScripts,
              event.id,
              requiredScriptHashes,
              validatorKeys,
              validityInterval,
              (transactionId, nativeScript, index) =>
                  IllegalArgumentException(
                    s"Invalid reference native script at index $index with scriptHash ${nativeScript.scriptHash} in transactionId $transactionId"
                  )
            )
            _ <- validateNativeScripts(
              providedNativeScripts,
              event.id,
              requiredScriptHashes,
              validatorKeys,
              validityInterval,
              (transactionId, nativeScript, index) =>
                  IllegalArgumentException(
                    s"Invalid provided native script at index $index with scriptHash ${nativeScript.scriptHash} in transactionId $transactionId"
                  )
            )
        yield ()
    }

    private def allValidatorKeys(
        event: Event
    ): Set[AddrKeyHash] = event.witnessSet.vkeyWitnesses.map(_.vkeyHash)

    private def extractValidityInterval(
        event: Event
    ): ValidityInterval = ValidityInterval(event.body.value.validityStartSlot, event.body.value.ttl)

    private def validateNativeScripts(
        nativeScripts: Set[Timelock],
        transactionId: TransactionHash,
        requiredScriptHashes: Set[ScriptHash],
        validatorKeys: Set[AddrKeyHash],
        validityInterval: ValidityInterval,
        invalidNativeScriptError: (TransactionHash, Timelock, Int) => IllegalArgumentException
    ): Result = boundary {
        for (nativeScript, index) <- nativeScripts.zipWithIndex
        do
            if requiredScriptHashes.contains(nativeScript.scriptHash) &&
                !nativeScript.evaluate(validatorKeys, validityInterval)
            then break(failure(invalidNativeScriptError(transactionId, nativeScript, index)))

        success
    }
}
