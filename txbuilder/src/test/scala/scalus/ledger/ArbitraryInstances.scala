package scalus.ledger

import scalus.builtin.ByteString
import org.scalacheck.{Arbitrary, Gen}
import ArbitraryDerivation.autoDerived

trait ArbitraryInstances {
    def genByteStringOfN(n: Int): Gen[ByteString] = {
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
        for
            size <- Gen.choose(0, 32)
            bytes <- Gen.containerOfN[Array, Byte](size, Arbitrary.arbitrary[Byte])
        yield AssetName(ByteString.unsafeFromArray(bytes))
    }
    given Arbitrary[Value] = autoDerived
    given Arbitrary[DRep] = autoDerived
    given Arbitrary[GovActionId] = Arbitrary {
        for
            txId <- Arbitrary.arbitrary[Hash32]
            index <- Gen.choose(0, 65535)
        yield GovActionId(txId, index)
    }
    given Arbitrary[OperationalCert] = Arbitrary {
        for
            hotVKey <- genByteStringOfN(32)
            sequenceNumber <- Gen.posNum[Long]
            kesPeriod <- Gen.posNum[Long]
            sigma <- genByteStringOfN(64)
        yield OperationalCert(hotVKey, sequenceNumber, kesPeriod, sigma)
    }
    given Arbitrary[PoolMetadata] = Arbitrary {
        for
            len <- Gen.choose(0, 128)
            url <- Gen.stringOfN(len, Gen.alphaNumChar)
            hash <- Arbitrary.arbitrary[Hash32]
        yield PoolMetadata(url, hash)
    }
}
