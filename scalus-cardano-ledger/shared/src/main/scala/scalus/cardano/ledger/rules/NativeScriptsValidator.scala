package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.utils.{AllNeededScriptHashesHelper, AllReferenceScriptsHelper, AllWitnessScriptsHelper}
import scalus.ledger.api.{Timelock, ValidityInterval}

import scala.util.boundary
import scala.util.boundary.break

// It's validateFailedBabbageScripts in cardano-ledger
object NativeScriptsValidator extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = {
        for
            neededScriptHashes <- AllNeededScriptHashesHelper.allNeededScriptHashes(
              state.utxo,
              event
            )
            referenceNativeScripts <- AllReferenceScriptsHelper.allReferenceNativeScripts(
              state.utxo,
              event
            )
            providedNativeScripts = AllWitnessScriptsHelper.allWitnessNativeScripts(event)
            validatorKeys = allValidatorKeys(event)
            validityInterval = extractValidityInterval(event)
            _ <- validateNativeScripts(
              referenceNativeScripts,
              event.id,
              neededScriptHashes,
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
              neededScriptHashes,
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
        nativeScripts: Set[Script.Native],
        transactionId: TransactionHash,
        neededScriptHashes: Set[ScriptHash],
        validatorKeys: Set[AddrKeyHash],
        validityInterval: ValidityInterval,
        invalidNativeScriptError: (TransactionHash, Timelock, Int) => IllegalArgumentException
    ): Result = boundary {
        for (nativeScript, index) <- nativeScripts.zipWithIndex
        do
            if neededScriptHashes.contains(nativeScript.scriptHash) &&
                !nativeScript.script.evaluate(validatorKeys, validityInterval)
            then break(failure(invalidNativeScriptError(transactionId, nativeScript.script, index)))

        success
    }
}
