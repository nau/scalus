package scalus.cardano.ledger

import org.scalacheck.{Arbitrary, Gen}
import scalus.builtin.{ByteString, Data}
import scalus.builtin.Data.*
import scalus.cardano.ledger.ArbitraryDerivation.autoDerived
import scalus.ledger.api.{KeyHash, SlotNo, Timelock}
import scalus.{builtin, uplc}

trait ArbitraryInstances extends uplc.ArbitraryInstances {
    def genByteStringOfN(n: Int): Gen[ByteString] = {
        Gen
            .containerOfN[Array, Byte](n, Arbitrary.arbitrary[Byte])
            .map(a => ByteString.unsafeFromArray(a))
    }

    def genMapOfN[A: Arbitrary, B: Arbitrary](n: Int): Gen[scala.collection.immutable.Map[A, B]] = {
        Gen.mapOfN(
          n,
          for
              key <- summon[Arbitrary[A]].arbitrary
              value <- summon[Arbitrary[B]].arbitrary
          yield (key, value)
        )
    }

    given Arbitrary[Hash28] = Arbitrary(genByteStringOfN(28).map(Hash28.apply))
    given Arbitrary[Hash32] = Arbitrary(genByteStringOfN(32).map(Hash32.apply))
    given Arbitrary[AddrKeyHash] = autoDerived
    given Arbitrary[ScriptHash] = autoDerived
    given Arbitrary[Anchor] = autoDerived
    given Arbitrary[Credential] = autoDerived
    given Arbitrary[Coin] = Arbitrary(Gen.choose(0L, Long.MaxValue).map(Coin.apply))

    given Arbitrary[AssetName] = Arbitrary {
        for
            size <- Gen.choose(0, 32)
            bs <- genByteStringOfN(size)
        yield AssetName(bs)
    }

    given Arbitrary[Mint] = {
        import scala.collection.immutable.Map

        given [A: Arbitrary, B: Arbitrary]: Arbitrary[Map[A, B]] =
            Arbitrary(Gen.choose(0, 8).flatMap(genMapOfN))

        summon[Arbitrary[Mint]]
    }

    given Arbitrary[Language] = autoDerived
    given Arbitrary[Address] = autoDerived
    given Arbitrary[Slot] = Arbitrary(Gen.choose(0L, Long.MaxValue).map(Slot.apply))

    given Arbitrary[ExUnits] = Arbitrary {
        for {
            mem <- Gen.choose(0L, Long.MaxValue)
            steps <- Gen.choose(0L, Long.MaxValue)
        } yield ExUnits(mem, steps)
    }

    given Arbitrary[ExUnitPrices] = autoDerived

    given Arbitrary[CostModels] = {
        import scala.collection.immutable.{List, Map}

        given [A: Arbitrary, B: Arbitrary]: Arbitrary[Map[A, B]] =
            Arbitrary(Gen.choose(0, 8).flatMap(genMapOfN))

        given [A: Arbitrary]: Arbitrary[List[A]] = Arbitrary(
          Gen.choose(0, 8).flatMap(Gen.listOfN(_, summon[Arbitrary[A]].arbitrary))
        )

        Arbitrary(summon[Arbitrary[Map[Int, List[Long]]]].arbitrary.map(CostModels.apply))
    }

    given Arbitrary[Constitution] = autoDerived
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
            if depth <= 0 then
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
    given Arbitrary[ProtocolVersion] = Arbitrary {
        for
            major <- Gen.choose(1, 10)
            minor <- Gen.choose(0, Int.MaxValue)
        yield ProtocolVersion(major, minor)
    }
    given Arbitrary[RewardAccount] = autoDerived

    given Arbitrary[NonNegativeInterval] = Arbitrary {
        for
            numerator <- Gen.choose(0L, Long.MaxValue)
            denominator <- Gen.posNum[Long]
        yield NonNegativeInterval(numerator, denominator)
    }

    given Arbitrary[VrfCert] = Arbitrary {
        for
            outputSize <- Gen.choose(0, 128)
            output <- genByteStringOfN(outputSize)
            proof <- genByteStringOfN(80)
        yield VrfCert(output, proof)
    }

    given Arbitrary[BlockHeaderBody] = Arbitrary {
        for
            blockNumber <- Gen.choose(0L, Long.MaxValue)
            slot <- Gen.choose(0L, Long.MaxValue)
            prevHash <- summon[Arbitrary[Option[Hash32]]].arbitrary
            issuerVkey <- genByteStringOfN(32)
            vrfVkey <- genByteStringOfN(32)
            vrfResult <- summon[Arbitrary[VrfCert]].arbitrary
            blockBodySize <- Gen.choose(0L, Long.MaxValue)
            blockBodyHash <- summon[Arbitrary[Hash32]].arbitrary
            operationalCert <- summon[Arbitrary[OperationalCert]].arbitrary
            protocolVersion <- summon[Arbitrary[ProtocolVersion]].arbitrary
        yield BlockHeaderBody(
          blockNumber,
          slot,
          prevHash,
          issuerVkey,
          vrfVkey,
          vrfResult,
          blockBodySize,
          blockBodyHash,
          operationalCert,
          protocolVersion
        )
    }

    given Arbitrary[BlockHeader] = Arbitrary {
        for
            headerBody <- summon[Arbitrary[BlockHeaderBody]].arbitrary
            bodySignature <- genByteStringOfN(448)
        yield BlockHeader(headerBody, bodySignature)
    }
}
