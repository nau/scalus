package scalus.ledger

import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.builtin.Data.*
import scalus.uplc
import org.scalacheck.{Arbitrary, Gen}
import ArbitraryDerivation.autoDerived
import scalus.builtin
import scalus.ledger.api.{KeyHash, SlotNo, Timelock}

trait ArbitraryInstances extends uplc.ArbitraryInstances {
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
            bs <- genByteStringOfN(size)
        yield AssetName(bs)
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
    given Arbitrary[KeyHash] = Arbitrary(
      Arbitrary.arbitrary[Hash28].map(h => KeyHash.apply(h.bytes))
    )
    given Arbitrary[DatumOption] = autoDerived

    object TimelockGen {

        /** KeyHash generator - simplified implementation assuming KeyHash is a value class wrapping
          * a byte array of fixed length (28 bytes as per Cardano specs)
          */
        val genKeyHash: Gen[KeyHash] = Arbitrary.arbitrary[Hash28].map(_.bytes).map(KeyHash.apply)

        /** SlotNo generator - simplified implementation assuming SlotNo is a value class wrapping a
          * Long
          */
        val genSlotNo: Gen[SlotNo] = Gen.choose(0L, Long.MaxValue)

        /** Main Timelock generator with controlled recursion depth Uses Gen.sized to ensure
          * termination and reasonable test data
          */
        val genTimelock: Gen[Timelock] = Gen.sized { size =>
            genTimelockWithDepth(size.min(3)) // Cap maximum depth at 3 as per requirements
        }

        /** Helper method to generate Timelock instances with controlled recursion depth
          *
          * @param depth
          *   Maximum nesting depth for recursive Timelock structures
          * @return
          *   A generator for Timelock instances
          */
        private def genTimelockWithDepth(depth: Int): Gen[Timelock] = {
            if (depth <= 0) then
                // Base cases - non-recursive Timelock variants
                Gen.oneOf(
                  genKeyHash.map(Timelock.Signature.apply),
                  genSlotNo.map(Timelock.TimeStart.apply),
                  genSlotNo.map(Timelock.TimeExpire.apply)
                )
            else
                // Calculate a reduced depth for nested generators
                val reducedDepth = depth - 1

                // Generate a list of nested Timelock instances with reduced depth
                lazy val genNestedTimelocks: Gen[Seq[Timelock]] = for
                    n <- Gen.choose(1, 5) // Reasonable limit for number of nested scripts
                    scripts <- Gen.listOfN(n, genTimelockWithDepth(reducedDepth))
                yield scripts

                // Choose between all possible Timelock variants
                Gen.frequency(
                  // Non-recursive variants have higher probability
                  (3, genKeyHash.map(Timelock.Signature.apply)),
                  (2, genSlotNo.map(Timelock.TimeStart.apply)),
                  (2, genSlotNo.map(Timelock.TimeExpire.apply)),

                  // Recursive variants with reduced depth
                  (1, genNestedTimelocks.map(Timelock.AllOf.apply)),
                  (1, genNestedTimelocks.map(Timelock.AnyOf.apply)),
                  (
                    1,
                    for
                        scripts <- genNestedTimelocks
                        m <- Gen.choose(1, scripts.length.max(1)) // Ensure m is valid
                    yield Timelock.MOf(m, scripts)
                  )
                )
        }
    }
    given Arbitrary[Script] = autoDerived
    given Arbitrary[ScriptRef] = autoDerived
    given Arbitrary[Timelock] = Arbitrary(TimelockGen.genTimelock)
    given Arbitrary[TransactionInput] = autoDerived
    given Arbitrary[TransactionOutput] = autoDerived
}
