package scalus.ledger

import scalus.builtin.ByteString
import org.scalacheck.{Arbitrary, Gen}

trait ArbitraryInstances {
    private def genByteStringOfN(n: Int): Gen[ByteString] = {
        Gen
            .containerOfN[Array, Byte](n, Arbitrary.arbitrary[Byte])
            .map(a => ByteString.unsafeFromArray(a))
    }

    given Arbitrary[Hash28] = Arbitrary(genByteStringOfN(28).map(Hash28.apply))
    given Arbitrary[Hash32] = Arbitrary(genByteStringOfN(32).map(Hash32.apply))
    given Arbitrary[AddrKeyHash] = ArbitraryDerivation.autoDerived[AddrKeyHash]
}
