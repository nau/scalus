package scalus.ledger.api.v2

import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary
import scalus.*
import scalus.builtin.Data
import scalus.cardano.ledger.DataHash
import scalus.ledger.api.v1

object ArbitraryInstances extends ArbitraryInstances
trait ArbitraryInstances extends v1.ArbitraryInstances {
    import scalus.cardano.ledger.ArbitraryInstances.given
    given Arbitrary[OutputDatum] = Arbitrary {
        Gen.oneOf(
          Gen.const(OutputDatum.NoOutputDatum),
          arbitrary[DataHash].map(OutputDatum.OutputDatumHash.apply),
          arbitrary[Data].map(OutputDatum.OutputDatum.apply)
        )
    }
}
