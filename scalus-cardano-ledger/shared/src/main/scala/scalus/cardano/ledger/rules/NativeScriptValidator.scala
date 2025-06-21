package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.rules.utils.*
import scalus.ledger.api.{Timelock, ValidityInterval}
import scala.util.boundary
import scala.util.boundary.break

// It's validateFailedBabbageScripts in cardano-ledger
object NativeScriptValidator
    extends STS.Validator,
      AllRequiredScriptHashes,
      AllReferenceScripts,
      AllProvidedScripts {
    override def validate(context: Context, state: State, event: Event): Result = boundary {
        for
            requiredScriptHashes <- allRequiredScriptHashes(state, event)
            referenceNativeScripts <- allReferenceNativeScripts(state, event)
            providedNativeScripts = allProvidedNativeScripts(event)
            nativeScripts = referenceNativeScripts.view ++ providedNativeScripts
            validatorKeys = allValidatorKeys(event)
            validityInterval = extractValidityInterval(event)
            _ <-
                for nativeScript <- nativeScripts
                do
                    if requiredScriptHashes.contains(nativeScript.scriptHash) &&
                        !nativeScript.evaluate(validatorKeys, validityInterval)
                    then
                        break(
                          failure(
                            IllegalArgumentException(
                              s"Invalid native script with scriptHash ${nativeScript.scriptHash} in transactionId ${event.id}"
                            )
                          )
                        )

                success
        yield ()
    }

    private def allValidatorKeys(
        event: Event
    ): Set[AddrKeyHash] = event.witnessSet.vkeyWitnesses.map(_.vkeyHash.asInstanceOf[AddrKeyHash])

    private def extractValidityInterval(
        event: Event
    ): ValidityInterval = ValidityInterval(event.body.value.validityStartSlot, event.body.value.ttl)
}
