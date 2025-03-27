package scalus.ledger

import scalus.builtin.ByteString
import org.scalacheck.{Arbitrary, Gen}
import ArbitraryDerivation.autoDerived

trait ArbitraryInstances {
    private def genByteStringOfN(n: Int): Gen[ByteString] = {
        Gen
            .containerOfN[Array, Byte](n, Arbitrary.arbitrary[Byte])
            .map(a => ByteString.unsafeFromArray(a))
    }

    given Arbitrary[Hash28] = Arbitrary(genByteStringOfN(28).map(Hash28.apply))
    given Arbitrary[Hash32] = Arbitrary(genByteStringOfN(32).map(Hash32.apply))
    given Arbitrary[AddrKeyHash] = autoDerived
    given Arbitrary[ScriptHash] = autoDerived
    given Arbitrary[Anchor] = autoDerived
    given Arbitrary[Credential] = autoDerived
    given Arbitrary[Coin] = Arbitrary(Gen.posNum[Long].map(Coin.apply))
    given Arbitrary[AssetName] = Arbitrary {
        for size <- Gen.choose(0, 32)
            Gen.containerOfN[Array, Byte](size, Arbitrary.arbitrary[Byte])
            .map(a => ByteString.unsafeFromArray(a))
    }
    given Arbitrary[Value] = autoDerived
}
