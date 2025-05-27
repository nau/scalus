package scalus.ledger.api
import io.bullet.borer.{Cbor, Decoder, Encoder}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.builtin.ByteString
import scalus.builtin.ByteString.hex

class TimelockTest extends AnyFunSuite with ScalaCheckPropertyChecks:

    // Helper method to create KeyHash for testing
    private def keyHash(hex: String): KeyHash = KeyHash(ByteString.fromHex(hex))

    // Define generators for our types
    private val byteStringGen: Gen[ByteString] =
        Gen.containerOf[Array, Byte](Arbitrary.arbByte.arbitrary)
            .map(ByteString.unsafeFromArray)

    private val keyHashGen: Gen[KeyHash] = byteStringGen.map(KeyHash.apply)

    private val slotNoGen: Gen[SlotNo] = Gen.chooseNum(0L, Long.MaxValue)

    private val optionSlotNoGen: Gen[Option[SlotNo]] = Gen.option(slotNoGen)

    private val validityIntervalGen: Gen[ValidityInterval] =
        for
            invalidBefore <- optionSlotNoGen
            invalidHereafter <- optionSlotNoGen
        yield ValidityInterval(invalidBefore, invalidHereafter)

    // Recursive generator for Timelock
    private val timelockGen: Gen[Timelock] =
        Gen.sized: size =>
            if size <= 1 then
                // Base cases for small sizes
                Gen.oneOf(
                  keyHashGen.map(Timelock.Signature.apply),
                  slotNoGen.map(Timelock.TimeStart.apply),
                  slotNoGen.map(Timelock.TimeExpire.apply)
                )
            else
                // Recursive cases with smaller sizes for nested scripts
                val smallerScripts = Gen.listOf(Gen.resize(size / 3, timelockGen))

                Gen.oneOf(
                  keyHashGen.map(Timelock.Signature.apply),
                  smallerScripts.map(scripts => Timelock.AllOf(scripts)),
                  smallerScripts.map(scripts => Timelock.AnyOf(scripts)),
                  for
                      m <- Gen.chooseNum(0, 10)
                      scripts <- smallerScripts
                  yield Timelock.MOf(m, scripts),
                  slotNoGen.map(Timelock.TimeStart.apply),
                  slotNoGen.map(Timelock.TimeExpire.apply)
                )

    // Implicit Arbitrary instances using given syntax
    private given Arbitrary[ByteString] = Arbitrary(byteStringGen)
    private given Arbitrary[KeyHash] = Arbitrary(keyHashGen)
    private given Arbitrary[SlotNo] = Arbitrary(slotNoGen)
    private given Arbitrary[ValidityInterval] = Arbitrary(validityIntervalGen)
    private given Arbitrary[Timelock] = Arbitrary(timelockGen)

    // Test evaluate method
    test("Signature with matching key hash should validate"):
        val hash1 = keyHash("deadbeef")
        val hash2 = keyHash("cafebabe")
        val validatorKeys = Set(hash1, hash2)

        val script = Timelock.Signature(hash1)
        val interval = ValidityInterval(None, None)

        assert(script.evaluate(validatorKeys, interval))

    test("Signature with non-matching key hash should be rejected"):
        val hash1 = keyHash("deadbeef")
        val hash2 = keyHash("cafebabe")
        val hash3 = keyHash("01020304")
        val validatorKeys = Set(hash1, hash2)

        val script = Timelock.Signature(hash3)
        val interval = ValidityInterval(None, None)

        assert(!script.evaluate(validatorKeys, interval))

    test("AllOf should validate when all scripts are valid"):
        val hash1 = keyHash("deadbeef")
        val hash2 = keyHash("cafebabe")
        val validatorKeys = Set(hash1, hash2)

        val script = Timelock.AllOf(
          Seq(
            Timelock.Signature(hash1),
            Timelock.Signature(hash2)
          )
        )
        val interval = ValidityInterval(None, None)

        assert(script.evaluate(validatorKeys, interval))

    test("AllOf should be rejected when any script is invalid"):
        val hash1 = keyHash("deadbeef")
        val hash2 = keyHash("cafebabe")
        val hash3 = keyHash("01020304")
        val validatorKeys = Set(hash1, hash2)

        val script = Timelock.AllOf(
          Seq(
            Timelock.Signature(hash1),
            Timelock.Signature(hash3) // Not in validatorKeys
          )
        )
        val interval = ValidityInterval(None, None)

        assert(!script.evaluate(validatorKeys, interval))

    test("AnyOf should validate when at least one script is valid"):
        val hash1 = keyHash("deadbeef")
        val hash2 = keyHash("cafebabe")
        val hash3 = keyHash("01020304")
        val validatorKeys = Set(hash1, hash2)

        val script = Timelock.AnyOf(
          Seq(
            Timelock.Signature(hash3), // Not in validatorKeys
            Timelock.Signature(hash2) // In validatorKeys
          )
        )
        val interval = ValidityInterval(None, None)

        assert(script.evaluate(validatorKeys, interval))

    test("AnyOf should be rejected when all scripts are invalid"):
        val hash1 = keyHash("deadbeef")
        val hash2 = keyHash("cafebabe")
        val hash3 = keyHash("01020304")
        val hash4 = keyHash("05060708")
        val validatorKeys = Set(hash1, hash2)

        val script = Timelock.AnyOf(
          Seq(
            Timelock.Signature(hash3), // Not in validatorKeys
            Timelock.Signature(hash4) // Not in validatorKeys
          )
        )
        val interval = ValidityInterval(None, None)

        assert(!script.evaluate(validatorKeys, interval))

    test("MOf should validate when enough scripts are valid"):
        val hash1 = keyHash("deadbeef")
        val hash2 = keyHash("cafebabe")
        val hash3 = keyHash("01020304")
        val hash4 = keyHash("05060708")
        val validatorKeys = Set(hash1, hash2, hash3)

        val script = Timelock.MOf(
          2,
          Seq(
            Timelock.Signature(hash1), // Valid
            Timelock.Signature(hash4), // Invalid
            Timelock.Signature(hash2) // Valid
          )
        )
        val interval = ValidityInterval(None, None)

        assert(script.evaluate(validatorKeys, interval))

    test("MOf should be rejected when not enough scripts are valid"):
        val hash1 = keyHash("deadbeef")
        val hash2 = keyHash("cafebabe")
        val hash3 = keyHash("01020304")
        val hash4 = keyHash("05060708")
        val validatorKeys = Set(hash1)

        val script = Timelock.MOf(
          2,
          Seq(
            Timelock.Signature(hash1), // Valid
            Timelock.Signature(hash4), // Invalid
            Timelock.Signature(hash2) // Invalid
          )
        )
        val interval = ValidityInterval(None, None)

        assert(!script.evaluate(validatorKeys, interval))

    test("MOf with m=0 should always validate"):
        val hash1 = keyHash("deadbeef")
        val validatorKeys = Set.empty[KeyHash]

        val script = Timelock.MOf(
          0,
          Seq(
            Timelock.Signature(hash1) // Invalid
          )
        )
        val interval = ValidityInterval(None, None)

        assert(script.evaluate(validatorKeys, interval))

    test("TimeStart should validate when slot is valid"):
        val interval = ValidityInterval(Some(100L), None)

        // Slot is <= invalidBefore
        val script1 = Timelock.TimeStart(50L)
        assert(script1.evaluate(Set.empty, interval))

        // Slot is == invalidBefore
        val script2 = Timelock.TimeStart(100L)
        assert(script2.evaluate(Set.empty, interval))

    test("TimeStart should be rejected when slot is invalid"):
        val interval = ValidityInterval(Some(100L), None)

        // Slot is > invalidBefore
        val script = Timelock.TimeStart(150L)
        assert(!script.evaluate(Set.empty, interval))

        // invalidBefore is None (negative infinity)
        val intervalWithNone = ValidityInterval(None, None)
        assert(!script.evaluate(Set.empty, intervalWithNone))

    test("TimeExpire should validate when slot is valid"):
        val interval = ValidityInterval(None, Some(100L))

        // invalidHereafter <= slot
        val script1 = Timelock.TimeExpire(150L)
        assert(script1.evaluate(Set.empty, interval))

        // invalidHereafter == slot
        val script2 = Timelock.TimeExpire(100L)
        assert(script2.evaluate(Set.empty, interval))

    test("TimeExpire should be rejected when slot is invalid"):
        val interval = ValidityInterval(None, Some(100L))

        // invalidHereafter > slot
        val script = Timelock.TimeExpire(50L)
        assert(!script.evaluate(Set.empty, interval))

        // invalidHereafter is None (positive infinity)
        val intervalWithNone = ValidityInterval(None, None)
        assert(!script.evaluate(Set.empty, intervalWithNone))

    test("Complex nested scripts should evaluate correctly"):
        val hash1 = keyHash("deadbeef")
        val hash2 = keyHash("cafebabe")
        val hash3 = keyHash("01020304")
        val validatorKeys = Set(hash1, hash2)

        val interval = ValidityInterval(Some(50L), Some(150L))

        // Complex script: AllOf(Signature(hash1), AnyOf(TimeStart(40), Signature(hash3)), TimeExpire(200))
        val script = Timelock.AllOf(
          Seq(
            Timelock.Signature(hash1), // Valid
            Timelock.AnyOf(
              Seq(
                Timelock.TimeStart(40L), // Valid (40 <= 50)
                Timelock.Signature(hash3) // Invalid
              )
            ),
            Timelock.TimeExpire(200L) // Valid (150 <= 200)
          )
        )

        assert(script.evaluate(validatorKeys, interval))

        // Change one condition to make it fail
        val invalidInterval = ValidityInterval(Some(30L), Some(150L))
        assert(!script.evaluate(validatorKeys, invalidInterval))

    // Property-based tests
    test("Scripts with all valid conditions should always validate"):
        forAll: (invalidBefore: Option[Long], invalidHereafter: Option[Long]) =>
            val interval = ValidityInterval(invalidBefore, invalidHereafter)
            val hash = keyHash("deadbeef")
            val validatorKeys = Set(hash)

            // Simple script that should always validate
            val script = Timelock.Signature(hash)

            assert(script.evaluate(validatorKeys, interval))

    test("Scripts with all invalid conditions should never validate"):
        forAll: (invalidBefore: Option[Long], invalidHereafter: Option[Long]) =>
            val interval = ValidityInterval(invalidBefore, invalidHereafter)
            val hash1 = keyHash("deadbeef")
            val hash2 = keyHash("cafebabe")
            val validatorKeys = Set(hash1)

            // Simple script that should never validate
            val script = Timelock.Signature(hash2)

            assert(!script.evaluate(validatorKeys, interval))

    test("inInterval should correctly check if a slot is in the validity interval"):
        // Both bounds are None
        assert(Timelock.inInterval(100L, ValidityInterval(None, None)))

        // Lower bound only
        assert(Timelock.inInterval(100L, ValidityInterval(Some(50L), None)))
        assert(Timelock.inInterval(100L, ValidityInterval(Some(100L), None)))
        assert(!Timelock.inInterval(100L, ValidityInterval(Some(150L), None)))

        // Upper bound only
        assert(Timelock.inInterval(100L, ValidityInterval(None, Some(150L))))
        assert(!Timelock.inInterval(100L, ValidityInterval(None, Some(100L))))
        assert(!Timelock.inInterval(100L, ValidityInterval(None, Some(50L))))

        // Both bounds
        assert(Timelock.inInterval(100L, ValidityInterval(Some(50L), Some(150L))))
        assert(Timelock.inInterval(100L, ValidityInterval(Some(100L), Some(150L))))
        assert(!Timelock.inInterval(100L, ValidityInterval(Some(50L), Some(100L))))
        assert(!Timelock.inInterval(100L, ValidityInterval(Some(150L), Some(200L))))

    test("lteNegInfty should correctly implement less-than-equal with negative infinity"):
        assert(!Timelock.lteNegInfty(100L, None))
        assert(!Timelock.lteNegInfty(100L, Some(50L)))
        assert(Timelock.lteNegInfty(100L, Some(100L)))
        assert(Timelock.lteNegInfty(100L, Some(150L)))

    test("ltePosInfty should correctly implement less-than-equal with positive infinity"):
        assert(!Timelock.ltePosInfty(None, 100L))
        assert(Timelock.ltePosInfty(Some(50L), 100L))
        assert(Timelock.ltePosInfty(Some(100L), 100L))
        assert(!Timelock.ltePosInfty(Some(150L), 100L))

    test("KeyHash round-trip encode and decode"):
        forAll: (keyHash: KeyHash) =>
            val encoded = Cbor.encode(keyHash).toByteArray
            val decoded = Cbor.decode(encoded).to[KeyHash].value
            assert(decoded == keyHash)

    test("Timelock.Signature round-trip encode and decode"):
        forAll: (keyHash: KeyHash) =>
            val timelock = Timelock.Signature(keyHash)
            val encoded = encode(timelock)
            val decoded = decode[Timelock](encoded)
            assert(decoded == timelock)

    test("Timelock.TimeStart round-trip encode and decode"):
        forAll: (slot: SlotNo) =>
            val timelock = Timelock.TimeStart(slot)
            val encoded = encode(timelock)
            val decoded = decode[Timelock](encoded)
            assert(decoded == timelock)

    test("Timelock.TimeExpire round-trip encode and decode"):
        forAll: (slot: SlotNo) =>
            val timelock = Timelock.TimeExpire(slot)
            val encoded = encode(timelock)
            val decoded = decode[Timelock](encoded)
            assert(decoded == timelock)

    test("Complex Timelock structures round-trip encode and decode"):
        // Focus on testing more complex cases explicitly
        val testCases = Seq(
          Timelock.AllOf(
            Seq(
              Timelock.Signature(KeyHash(hex"deadbeef")),
              Timelock.TimeStart(123L)
            )
          ),
          Timelock.AnyOf(
            Seq(
              Timelock.Signature(KeyHash(hex"cafebabe")),
              Timelock.TimeExpire(456L)
            )
          ),
          Timelock.MOf(
            2,
            Seq(
              Timelock.Signature(KeyHash(hex"01020304")),
              Timelock.Signature(KeyHash(hex"05060708")),
              Timelock.Signature(KeyHash(hex"090a0b0c"))
            )
          )
        )

        for timelock <- testCases do
            val encoded = encode(timelock)
            val decoded = decode[Timelock](encoded)
            assert(decoded == timelock)

    test("Any Timelock structure round-trip encode and decode"):
        forAll(Gen.resize(5, timelockGen)): timelock =>
            val encoded = encode(timelock)
            val decoded = decode[Timelock](encoded)
            assert(decoded == timelock)

    test("Deeply nested Timelock structures encode and decode correctly"):
        // Create a deeply nested structure
        def createNestedTimelock(depth: Int): Timelock =
            if depth <= 0 then Timelock.Signature(KeyHash(hex"deadbeef"))
            else
                Timelock.AllOf(
                  Seq(
                    createNestedTimelock(depth - 1),
                    Timelock.AnyOf(
                      Seq(
                        Timelock.TimeStart(depth.toLong),
                        createNestedTimelock(depth - 1)
                      )
                    )
                  )
                )

        val deepTimelock = createNestedTimelock(5)
        val encoded = encode(deepTimelock)
        val decoded = decode[Timelock](encoded)
        assert(decoded == deepTimelock)

    private def encode(value: Timelock): Array[Byte] = value.toCbor
    private def decode[A: Decoder](bytes: Array[Byte]): Timelock = Timelock.fromCbor(bytes)
