package scalus.ledger.api.v1

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import scalus.*
import scalus.prelude.Ord

import scalus.builtin.ByteString
import scalus.uplc.test

trait ArbitraryInstances extends test.ArbitraryInstances:
    given Arbitrary[IntervalBoundType] = Arbitrary {
        Gen.frequency(
          (1, Gen.const(IntervalBoundType.NegInf)),
          (1, Gen.const(IntervalBoundType.PosInf)),
          (8, Arbitrary.arbitrary[PosixTime].map(IntervalBoundType.Finite(_)))
        )
    }

    given Arbitrary[IntervalBound] = Arbitrary {
        for
            time <- Arbitrary.arbitrary[IntervalBoundType]
            closure <- Arbitrary.arbitrary[Boolean]
        yield IntervalBound(time, closure)
    }

    given Arbitrary[Interval] = Arbitrary {
        for
            b1 <- Arbitrary.arbitrary[IntervalBound]
            b2 <- Arbitrary.arbitrary[IntervalBound]
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
