package scalus.ledger.api.v2

import org.scalacheck.Arbitrary
import scalus.ledger.api.v1
import scalus.testutil.ArbitraryDerivation.autoDerived

object ArbitraryInstances extends ArbitraryInstances
trait ArbitraryInstances extends v1.ArbitraryInstances {
    import scalus.cardano.ledger.ArbitraryInstances.given
    given Arbitrary[OutputDatum] = autoDerived
}
