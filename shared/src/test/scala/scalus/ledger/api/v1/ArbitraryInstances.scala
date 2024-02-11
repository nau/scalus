package scalus.ledger.api.v1

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import scalus.*
import scalus.builtin

import scalus.builtin.ByteString

trait ArbitraryInstances extends scalus.uplc.ArbitraryInstances:
    given Arbitrary[Extended[POSIXTime]] = Arbitrary {
        Gen.frequency(
          (1, Gen.const(Extended.NegInf)),
          (1, Gen.const(Extended.PosInf)),
          (8, Arbitrary.arbitrary[POSIXTime].map(Extended.Finite(_)))
        )
    }

    given Arbitrary[LowerBound[POSIXTime]] = Arbitrary {
        for
            time <- Arbitrary.arbitrary[Extended[POSIXTime]]
            closure <- Arbitrary.arbitrary[Boolean]
        yield LowerBound(time, closure)
    }

    given Arbitrary[UpperBound[POSIXTime]] = Arbitrary {
        for
            time <- Arbitrary.arbitrary[Extended[POSIXTime]]
            closure <- Arbitrary.arbitrary[Boolean]
        yield UpperBound(time, closure)
    }

    given Arbitrary[Interval[POSIXTime]] = Arbitrary {
        for
            lower <- Arbitrary.arbitrary[LowerBound[POSIXTime]]
            upper <- Arbitrary.arbitrary[UpperBound[POSIXTime]]
        yield Interval[POSIXTime](lower, upper)
    }

    given Arbitrary[TxId] = Arbitrary {
        Gen
            .containerOfN[Array, Byte](64, Arbitrary.arbitrary[Byte])
            .map(ba => TxId(ByteString.unsafeFromArray(ba)))
    }
