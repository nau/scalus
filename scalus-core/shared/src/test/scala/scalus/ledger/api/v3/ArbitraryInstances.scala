package scalus.ledger.api.v3

import org.scalacheck.{Arbitrary, Gen}
import scalus.*
import scalus.ledger.api.v2

object ArbitraryInstances extends ArbitraryInstances
trait ArbitraryInstances extends v2.ArbitraryInstances {
    given Arbitrary[TxId] = Arbitrary(genByteStringOfN(32).map(TxId.apply))
    given Arbitrary[TxOutRef] = Arbitrary {
        for
            txId <- Arbitrary.arbitrary[TxId]
            index <- Gen.frequency(
              (10, Gen.choose(0, 3)),
              (3, Gen.choose(4, 10)),
              (1, Gen.choose(3, 1000))
            )
        yield TxOutRef(txId, index)
    }

    given Arbitrary[TxInInfo] = Arbitrary {
        for
            outRef <- Arbitrary.arbitrary[TxOutRef]
            resolved <- Arbitrary.arbitrary[TxOut]
        yield TxInInfo(outRef, resolved)
    }

}
