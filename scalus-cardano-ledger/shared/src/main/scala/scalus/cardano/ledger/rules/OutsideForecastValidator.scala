package scalus.cardano.ledger
package rules

// It's Babbage.validateOutsideForecast in cardano-ledger
// TODO In our current implementation, SlotConfig operations always succeed, but in the original Haskell version,
//  EpochInfo does not. Below is the original comment
//-- | Information about epochs
//--
//-- Different epochs may have different sizes and different slot lengths. This
//-- information is encapsulated by 'EpochInfo'. It is parameterized over a monad
//-- @m@ because the information about how long each epoch is may depend on
//-- information derived from the blockchain itself. It ultimately requires acess
//-- to state, and so either uses the monad for that or uses the monad to reify
//-- failure due to cached state information being too stale for the current
//-- query.
object OutsideForecastValidator extends STS.Validator {
    override final type Error = Nothing

    override def validate(context: Context, state: State, event: Event): Result = {
        success
    }
}
