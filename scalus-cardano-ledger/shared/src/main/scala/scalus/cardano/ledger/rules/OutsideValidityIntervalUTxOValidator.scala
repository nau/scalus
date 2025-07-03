package scalus.cardano.ledger
package rules

import scalus.ledger.api.ValidityInterval

// It's Babbage.validateOutsideValidityIntervalUTxO in cardano-ledger
object OutsideValidityIntervalUTxOValidator extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionId = event.id
        val validityInterval = extractValidityInterval(event)
        val slot = context.env.slot

        if checkInterval(validityInterval, slot) then success
        else
            failure(
              IllegalArgumentException(
                s"Transaction $transactionId is outside the validity interval $validityInterval for slot $slot"
              )
            )
    }

    // Test if a slot is in the Validity interval. Recall that a ValidityInterval
    // is a half Open interval, that is why we use (slot < invalidHereafter)
    private def checkInterval(
        validityInterval: ValidityInterval,
        slot: SlotNo
    ): Boolean = {
        validityInterval match
            case ValidityInterval(None, None)                   => true
            case ValidityInterval(None, Some(invalidHereafter)) => slot < invalidHereafter
            case ValidityInterval(Some(invalidBefore), None)    => slot >= invalidBefore
            case ValidityInterval(Some(invalidBefore), Some(invalidHereafter)) =>
                slot >= invalidBefore && slot < invalidHereafter
    }

    private def extractValidityInterval(
        event: Event
    ): ValidityInterval = ValidityInterval(event.body.value.validityStartSlot, event.body.value.ttl)
}
