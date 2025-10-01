package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.serialization.cbor.Cbor

import java.math.BigInteger

class Word64Test extends AnyFunSuite with ScalaCheckPropertyChecks:

    test("Word64 construction and basic operations"):
        val zero = Word64.Zero
        val one = Word64.One
        val max = Word64.MaxValue

        assert(zero.toLong == 0L)
        assert(one.toLong == 1L)
        assert(max.toLong == -1L) // Two's complement representation of 2^64-1

        assert(zero.toUnsignedString == "0")
        assert(one.toUnsignedString == "1")
        assert(max.toUnsignedString == "18446744073709551615")

    test("Word64.fromUnsignedString"):
        assert(Word64.fromUnsignedString("0").toUnsignedString == "0")
        assert(Word64.fromUnsignedString("12345").toUnsignedString == "12345")
        assert(
          Word64.fromUnsignedString("9223372036854775807").toUnsignedString == "9223372036854775807"
        ) // Long.MAX_VALUE
        assert(
          Word64.fromUnsignedString("9223372036854775808").toUnsignedString == "9223372036854775808"
        ) // Long.MAX_VALUE + 1
        assert(
          Word64
              .fromUnsignedString("18446744073709551615")
              .toUnsignedString == "18446744073709551615"
        ) // 2^64 - 1

        assertThrows[NumberFormatException]:
            Word64.fromUnsignedString("-1")

        assertThrows[NumberFormatException]:
            Word64.fromUnsignedString("18446744073709551616") // 2^64

    test("Word64.fromBigInteger"):
        assert(Word64.fromBigInteger(BigInteger.ZERO).toUnsignedString == "0")
        assert(Word64.fromBigInteger(BigInteger.valueOf(12345L)).toUnsignedString == "12345")
        assert(
          Word64
              .fromBigInteger(BigInteger.valueOf(Long.MaxValue))
              .toUnsignedString == "9223372036854775807"
        )

        val maxUint64 = BigInteger.ONE.shiftLeft(64).subtract(BigInteger.ONE)
        assert(Word64.fromBigInteger(maxUint64).toUnsignedString == "18446744073709551615")

        assertThrows[IllegalArgumentException]:
            Word64.fromBigInteger(BigInteger.valueOf(-1L))

        assertThrows[IllegalArgumentException]:
            Word64.fromBigInteger(BigInteger.ONE.shiftLeft(64)) // 2^64

    test("Word64.fromUnsignedInt"):
        assert(Word64.fromUnsignedInt(0).toUnsignedString == "0")
        assert(Word64.fromUnsignedInt(Int.MaxValue).toUnsignedString == "2147483647")
        assert(Word64.fromUnsignedInt(-1).toUnsignedString == "4294967295") // 2^32 - 1

    test("toBigInteger"):
        assert(Word64.Zero.toBigInteger == BigInteger.ZERO)
        assert(Word64.One.toBigInteger == BigInteger.ONE)
        assert(
          Word64.MaxValue.toBigInteger == BigInteger.ONE.shiftLeft(64).subtract(BigInteger.ONE)
        )

        val largeValue = Word64.fromUnsignedString("12345678901234567890")
        assert(largeValue.toBigInteger == new BigInteger("12345678901234567890"))

    test("toHexString"):
        assert(Word64.Zero.toHexString == "0")
        assert(Word64.One.toHexString == "1")
        assert(Word64(255L).toHexString == "ff")
        assert(Word64.MaxValue.toHexString == "ffffffffffffffff")

    test("isLargeUnsigned"):
        assert(Word64.Zero.isLargeUnsigned == false)
        assert(Word64.One.isLargeUnsigned == false)
        assert(Word64(Long.MaxValue).isLargeUnsigned == false)
        assert(
          Word64.fromUnsignedString("9223372036854775808").isLargeUnsigned == true
        ) // Long.MAX_VALUE + 1
        assert(Word64.MaxValue.isLargeUnsigned == true)

    test("compareUnsigned"):
        val small = Word64(100L)
        val medium = Word64(Long.MaxValue)
        val large = Word64.fromUnsignedString("12345678901234567890")
        val max = Word64.MaxValue

        assert(small.compareUnsigned(small) == 0)
        assert(small.compareUnsigned(medium) < 0)
        assert(medium.compareUnsigned(large) < 0)
        assert(large.compareUnsigned(max) < 0)
        assert(max.compareUnsigned(small) > 0)

    test("arithmetic operations"):
        val a = Word64(1000L)
        val b = Word64(500L)

        assert((a + b).toUnsignedString == "1500")
        assert((a - b).toUnsignedString == "500")
        assert((a * b).toUnsignedString == "500000")
        assert(a.divideUnsigned(b).toUnsignedString == "2")
        assert(a.remainderUnsigned(b).toUnsignedString == "0")

        // Test with large values
        val large1 = Word64.fromUnsignedString("18446744073709551610") // 2^64 - 6
        val large2 = Word64.fromUnsignedString("5")

        assert((large1 + large2).toUnsignedString == "18446744073709551615") // 2^64 - 1

        // Test overflow wrapping
        val maxMinusOne = Word64.fromUnsignedString("18446744073709551614") // 2^64 - 2
        val two = Word64(2L)
        assert((maxMinusOne + two).toUnsignedString == "0") // Wraps around

    test("bitwise operations"):
        val a = Word64(0xff00ff00ff00ff00L)
        val b = Word64(0x00ff00ff00ff00ffL)

        assert((a & b).toLong == 0L)
        assert((a | b).toLong == -1L) // All bits set
        assert((a ^ b).toLong == -1L) // All bits set
        assert((~Word64.Zero).toLong == -1L) // All bits set

        // Shift operations
        val value = Word64(0x123456789abcdef0L)
        assert((value << 4).toLong == 0x23456789abcdef00L)
        assert((value >>> 4).toLong == 0x0123456789abcdefL)

    test("division by zero throws exception"):
        assertThrows[ArithmeticException]:
            Word64.One.divideUnsigned(Word64.Zero)

        assertThrows[ArithmeticException]:
            Word64.One.remainderUnsigned(Word64.Zero)

    test("CBOR serialization round-trip for small values"):
        val testValues = List(
          Word64.Zero,
          Word64.One,
          Word64(12345L),
          Word64(Long.MaxValue)
        )

        for value <- testValues do
            val encoded = Cbor.encode(value)
            val decoded = Cbor.decode[Word64](encoded)
            assert(decoded.toUnsignedString == value.toUnsignedString)

    test("CBOR serialization round-trip for large values"):
        val testValues = List(
          Word64.fromUnsignedString("9223372036854775808"), // Long.MAX_VALUE + 1
          Word64.fromUnsignedString("12345678901234567890"),
          Word64.MaxValue
        )

        for value <- testValues do
            val encoded = Cbor.encode(value)
            val decoded = Cbor.decode[Word64](encoded)
            assert(decoded.toUnsignedString == value.toUnsignedString)

    test("CBOR encoding format"):
        // Small values should encode as regular CBOR integers
        val small = Word64(12345L)
        val smallEncoded = Cbor.encode(small)
        val smallAsLong = Cbor.encode(12345L)
        assert(smallEncoded sameElements smallAsLong)

        // Large values should encode as BigInteger
        val large = Word64.fromUnsignedString("12345678901234567890")
        val largeEncoded = Cbor.encode(large)
        val largeAsBigInt = Cbor.encode(new BigInteger("12345678901234567890"))
        assert(largeEncoded sameElements largeAsBigInt)

    // Property-based tests - using the Arbitrary instance from ArbitraryInstances

    test("property: toBigInteger is always non-negative"):
        forAll: (w: Word64) =>
            assert(w.toBigInteger.signum() >= 0)

    test("property: CBOR round-trip preserves value"):
        forAll: (w: Word64) =>
            val encoded = Cbor.encode(w)
            val decoded = Cbor.decode[Word64](encoded)
            assert(decoded.toUnsignedString == w.toUnsignedString)

    test("property: fromUnsignedString round-trip"):
        forAll: (w: Word64) =>
            val str = w.toUnsignedString
            val parsed = Word64.fromUnsignedString(str)
            assert(parsed.toUnsignedString == str)

    test("property: compareUnsigned is consistent with BigInteger comparison"):
        forAll: (w1: Word64, w2: Word64) =>
            val cmpWord64 = w1.compareUnsigned(w2)
            val cmpBigInt = w1.toBigInteger.compareTo(w2.toBigInteger)
            assert(cmpWord64.sign == cmpBigInt.sign)

    test("property: addition is consistent with BigInteger (modulo 2^64)"):
        forAll: (w1: Word64, w2: Word64) =>
            val sumWord64 = (w1 + w2).toBigInteger
            val sumBigInt =
                w1.toBigInteger.add(w2.toBigInteger).remainder(BigInteger.ONE.shiftLeft(64))
            assert(sumWord64 == sumBigInt)

    test("edge cases"):
        // Test boundary values
        val justOverMaxLong = Word64.fromUnsignedString("9223372036854775808") // Long.MAX_VALUE + 1
        assert(justOverMaxLong.isLargeUnsigned == true)
        assert(justOverMaxLong.toLong == Long.MinValue) // -9223372036854775808 in two's complement

        // Test that negative input to constructor is handled correctly
        val negativeAsUnsigned = Word64(-1L)
        assert(negativeAsUnsigned.toUnsignedString == "18446744073709551615") // 2^64 - 1
