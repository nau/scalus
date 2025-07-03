package scalus.cardano.ledger
package rules

// It's Babbage.validateOutsideForecast in cardano-ledger
// TODO recalculate slotConfig according to slot parameter
object OutsideForecastValidator extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = {
        val slotConfig = context.slotConfig
        val ttl = event.body.value.ttl
        val nonEmptyRedeemers = !event.witnessSet.redeemers.forall(_.value.isEmpty)

        ttl match
            case Some(slot) if nonEmptyRedeemers =>
                slotConfig.slotToTime(slot)
                ???
            case _ => success
    }
}
