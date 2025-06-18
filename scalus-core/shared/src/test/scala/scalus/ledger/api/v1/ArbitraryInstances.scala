package scalus.ledger.api.v1

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import scalus.*
import scalus.prelude.{Option, Ord}
import scalus.testutil.ArbitraryDerivation.autoDerived
import scalus.uplc.test

object ArbitraryInstances extends ArbitraryInstances
trait ArbitraryInstances extends test.ArbitraryInstances {
    given Arbitrary[IntervalBoundType] = Arbitrary {
        Gen.frequency(
          (1, Gen.const(IntervalBoundType.NegInf)),
          (1, Gen.const(IntervalBoundType.PosInf)),
          (8, arbitrary[PosixTime].map(IntervalBoundType.Finite(_)))
        )
    }

    given Arbitrary[IntervalBound] = autoDerived

    given Arbitrary[Interval] = Arbitrary {
        for
            b1 <- arbitrary[IntervalBound]
            b2 <- arbitrary[IntervalBound]
        yield summon[Ord[IntervalBound]].compare(b1, b2) match
            case Ord.Order.Greater => Interval(b2, b1)
            case _                 => Interval(b1, b2)
    }

    given Arbitrary[TxId] = Arbitrary {
        genByteStringOfN(32).map(TxId.apply)
    }

    given Arbitrary[PubKeyHash] = Arbitrary {
        genByteStringOfN(28).map(PubKeyHash.apply)
    }

    given Arbitrary[Credential] = autoDerived

    given Arbitrary[StakingCredential] = Arbitrary {
        // We don't generate StakingPtr because it's deprecated and can't be used since Conway era.
        arbitrary[Credential].map(StakingCredential.StakingHash.apply)
    }

    given Arbitrary[Address] = autoDerived
}
