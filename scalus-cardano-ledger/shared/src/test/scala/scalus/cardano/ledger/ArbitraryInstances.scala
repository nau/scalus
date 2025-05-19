package scalus.cardano.ledger

import org.scalacheck.{Arbitrary, Gen}
import scalus.builtin.{ByteString, Data}
import scalus.builtin.Data.*
import scalus.cardano.ledger.ArbitraryDerivation.autoDerived
import scalus.ledger.api.{KeyHash, SlotNo, Timelock}
import scalus.{builtin, uplc}
import scala.collection.immutable

trait ArbitraryInstances extends uplc.ArbitraryInstances {
    def genByteStringOfN(n: Int): Gen[ByteString] = {
        Gen
            .containerOfN[Array, Byte](n, Arbitrary.arbitrary[Byte])
            .map(a => ByteString.unsafeFromArray(a))
    }

    def genMapOfSizeFromArbitrary[A: Arbitrary, B: Arbitrary](
        from: Int,
        to: Int
    ): Gen[immutable.Map[A, B]] = {
        for
            size <- Gen.choose(from, to)
            result <- Gen.mapOfN(
              size,
              for
                  key <- Arbitrary.arbitrary[A]
                  value <- Arbitrary.arbitrary[B]
              yield (key, value)
            )
        yield result
    }

    def genListOfSizeFromArbitrary[A: Arbitrary](
        from: Int,
        to: Int
    ): Gen[immutable.List[A]] = {
        for
            size <- Gen.choose(from, to)
            result <- Gen.listOfN(size, Arbitrary.arbitrary[A])
        yield result
    }

    def genSetOfSizeFromArbitrary[A: Arbitrary](
        from: Int,
        to: Int
    ): Gen[immutable.Set[A]] = genListOfSizeFromArbitrary(from, to).map(_.toSet)

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
        given [A: Arbitrary, B: Arbitrary]: Arbitrary[immutable.Map[A, B]] = Arbitrary(
          genMapOfSizeFromArbitrary(0, 8)
        )

        summon[Arbitrary[Mint]]
    }

    given Arbitrary[Language] = autoDerived
    given Arbitrary[Address] = autoDerived
    given Arbitrary[Slot] = Arbitrary(Gen.choose(0L, Long.MaxValue).map(Slot.apply))

    given Arbitrary[ExUnits] = Arbitrary {
        for
            mem <- Gen.choose(0L, Long.MaxValue)
            steps <- Gen.choose(0L, Long.MaxValue)
        yield ExUnits(mem, steps)
    }

    given Arbitrary[ExUnitPrices] = autoDerived

    given Arbitrary[CostModels] = {
        given [A: Arbitrary, B: Arbitrary]: Arbitrary[immutable.Map[A, B]] = Arbitrary(
          genMapOfSizeFromArbitrary(0, 8)
        )

        given [A: Arbitrary]: Arbitrary[immutable.List[A]] = Arbitrary(
          genListOfSizeFromArbitrary(0, 8)
        )

        given result: Arbitrary[CostModels] = autoDerived
        result
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

    given Arbitrary[TransactionInput] = Arbitrary {
        for
            transactionId <- Arbitrary.arbitrary[Hash32]
            index <- Gen.choose(0, Int.MaxValue)
        yield TransactionInput(transactionId, index)
    }

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
            prevHash <- Arbitrary.arbitrary[Option[Hash32]]
            issuerVkey <- genByteStringOfN(32)
            vrfVkey <- genByteStringOfN(32)
            vrfResult <- Arbitrary.arbitrary[VrfCert]
            blockBodySize <- Gen.choose(0L, Long.MaxValue)
            blockBodyHash <- Arbitrary.arbitrary[Hash32]
            operationalCert <- Arbitrary.arbitrary[OperationalCert]
            protocolVersion <- Arbitrary.arbitrary[ProtocolVersion]
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
            headerBody <- Arbitrary.arbitrary[BlockHeaderBody]
            bodySignature <- genByteStringOfN(448)
        yield BlockHeader(headerBody, bodySignature)
    }

    given Arbitrary[TransactionMetadatumLabel] = Arbitrary(
      Gen.choose(0L, Long.MaxValue).map(TransactionMetadatumLabel.apply)
    )

    given Arbitrary[TransactionMetadatum] = {
        val genTransactionMetadatumInt =
            Gen.choose(Long.MinValue, Long.MaxValue).map(TransactionMetadatum.Int.apply)

        val genTransactionMetadatumBytes =
            Gen.choose(0, 64).flatMap(genByteStringOfN).map(TransactionMetadatum.Bytes.apply)

        val genTransactionMetadatumText = Gen
            .choose(0, 64)
            .flatMap(Gen.listOfN(_, Gen.alphaNumChar))
            .map(_.mkString)
            .map(TransactionMetadatum.Text.apply)

        def genTransactionMetadatum(depth: Int): Gen[TransactionMetadatum] = {
            if depth <= 0 then
                Gen.oneOf(
                  Gen.const(
                    TransactionMetadatum.Map(
                      immutable.Map.empty[TransactionMetadatum, TransactionMetadatum]
                    )
                  ),
                  Gen.const(TransactionMetadatum.List(immutable.Seq.empty[TransactionMetadatum])),
                  genTransactionMetadatumInt,
                  genTransactionMetadatumBytes,
                  genTransactionMetadatumText
                )
            else
                val reducedDepth = depth - 1
                Gen.frequency(
                  2 -> (
                    for
                        size <- Gen.choose(0, 2)
                        result <- Gen.mapOfN(
                          size,
                          for
                              key <- Gen.lzy(genTransactionMetadatum(reducedDepth))
                              value <- Gen.lzy(genTransactionMetadatum(reducedDepth))
                          yield (key, value)
                        )
                    yield TransactionMetadatum.Map(result)
                  ),
                  2 -> (
                    for
                        size <- Gen.choose(0, 2)
                        result <- Gen.listOfN(size, Gen.lzy(genTransactionMetadatum(reducedDepth)))
                    yield TransactionMetadatum.List(result)
                  ),
                  1 -> genTransactionMetadatumInt,
                  1 -> genTransactionMetadatumBytes,
                  1 -> genTransactionMetadatumText
                )

        }

        Arbitrary(Gen.choose(0, 3).flatMap(genTransactionMetadatum))
    }

    given Arbitrary[AuxiliaryData] = {
        given [A: Arbitrary, B: Arbitrary]: Arbitrary[immutable.Map[A, B]] = Arbitrary(
          genMapOfSizeFromArbitrary(0, 4)
        )

        given [A: Arbitrary]: Arbitrary[immutable.List[A]] = Arbitrary(
          genListOfSizeFromArbitrary(0, 4)
        )

        given result: Arbitrary[AuxiliaryData] = autoDerived
        result
    }

    given Arbitrary[VKeyWitness] = Arbitrary {
        for
            vkey <- genByteStringOfN(32)
            signature <- genByteStringOfN(64)
        yield VKeyWitness(vkey, signature)
    }

    given Arbitrary[BootstrapWitness] = Arbitrary {
        for
            publicKey <- genByteStringOfN(32)
            signature <- genByteStringOfN(64)
            chainCode <- genByteStringOfN(32)
            attributesSize <- Gen.choose(0, 128)
            attributes <- genByteStringOfN(attributesSize)
        yield BootstrapWitness(publicKey, signature, chainCode, attributes)
    }

    given Arbitrary[RedeemerTag] = autoDerived

    given Arbitrary[Redeemer] = Arbitrary {
        for
            tag <- Arbitrary.arbitrary[RedeemerTag]
            index <- Gen.choose(0, Int.MaxValue)
            data <- Arbitrary.arbitrary[Data]
            exUnits <- Arbitrary.arbitrary[ExUnits]
        yield Redeemer(tag, index, data, exUnits)
    }

    given Arbitrary[TransactionWitnessSet] = {
        given [A: Arbitrary]: Arbitrary[immutable.Set[A]] = Arbitrary(
          genSetOfSizeFromArbitrary(1, 3)
        )

        given result: Arbitrary[TransactionWitnessSet] = autoDerived
        result
    }

    given Arbitrary[UnitInterval] = Arbitrary {
        for
            denominator <- Gen.posNum[Long]
            numerator <- Gen.choose(0L, denominator)
        yield UnitInterval(numerator, denominator)
    }

    given Arbitrary[ProposalProcedure] = autoDerived
    given Arbitrary[Vote] = autoDerived
    given Arbitrary[Voter] = autoDerived
    given Arbitrary[VotingProcedure] = autoDerived

    given Arbitrary[VotingProcedures] = {
        given [A: Arbitrary, B: Arbitrary]: Arbitrary[immutable.Map[A, B]] = Arbitrary(
          genMapOfSizeFromArbitrary(0, 8)
        )

        given result: Arbitrary[VotingProcedures] = autoDerived
        result
    }

    given Arbitrary[PoolVotingThresholds] = autoDerived
    given Arbitrary[DRepVotingThresholds] = autoDerived
    given Arbitrary[ProtocolParamUpdate] = autoDerived
    given Arbitrary[GovAction] = autoDerived
}
