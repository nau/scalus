package scalus.cardano.ledger

import io.bullet.borer.*
import upickle.default.{readwriter as upickleReadwriter, ReadWriter as UpickleReadWriter}

import scala.annotation.tailrec

/** Represents a non-negative interval in the Cardano blockchain.
  *
  * A non-negative interval is represented as a fraction with a numerator and denominator, where
  * both are non-negative and the denominator is positive.
  *
  * @param numerator
  *   The numerator of the fraction (non-negative)
  * @param denominator
  *   The denominator of the fraction (positive)
  */
case class NonNegativeInterval(numerator: Long, denominator: Long) {
    // Validate constraints
    require(numerator >= 0, "Numerator must be non-negative")
    require(denominator > 0, "Denominator must be positive")

    override def equals(obj: Any): Boolean = {
        obj match {
            case other: NonNegativeInterval =>
                val reducedThis = this.reduce
                val reducedOther = other.reduce
                reducedThis.numerator == reducedOther.numerator && reducedThis.denominator == reducedOther.denominator
            case _ => false
        }
    }

    /** Multiplication operation with Int.
      *
      * @param factor
      *   The Int to multiply by
      * @return
      *   The product as a reduced NonNegativeInterval
      * @throws IllegalArgumentException
      *   if factor is negative
      */
    def *(factor: Int): NonNegativeInterval = {
        require(factor >= 0, "Factor must be non-negative")
        if factor == 0 then {
            NonNegativeInterval.zero
        } else {
            val newNumerator = (BigInt(this.numerator) * BigInt(factor)).toLong
            NonNegativeInterval.reduce(newNumerator, this.denominator)
        }
    }

    /** Multiplication operation with Long.
      *
      * @param factor
      *   The Int to multiply by
      * @return
      *   The product as a reduced NonNegativeInterval
      * @throws IllegalArgumentException
      *   if factor is negative
      */
    def *(factor: Long): NonNegativeInterval = {
        require(factor >= 0, "Factor must be non-negative")
        if factor == 0 then {
            NonNegativeInterval.zero
        } else {
            val newNumerator = (BigInt(this.numerator) * BigInt(factor)).toLong
            NonNegativeInterval.reduce(newNumerator, this.denominator)
        }
    }

    /** Addition operation.
      *
      * @param other
      *   The other NonNegativeInterval to add
      * @return
      *   The sum as a reduced NonNegativeInterval
      */
    def +(other: NonNegativeInterval): NonNegativeInterval = {
        val newNumerator = (BigInt(this.numerator) * BigInt(other.denominator) + BigInt(
          other.numerator
        ) * BigInt(this.denominator)).toLong
        val newDenominator = (BigInt(this.denominator) * BigInt(other.denominator)).toLong
        NonNegativeInterval.reduce(newNumerator, newDenominator)
    }

    /** Multiplication operation.
      *
      * @param other
      *   The other NonNegativeInterval to multiply
      * @return
      *   The product as a reduced NonNegativeInterval
      */
    def *(other: NonNegativeInterval): NonNegativeInterval = {
        val newNumerator = (BigInt(this.numerator) * BigInt(other.numerator)).toLong
        val newDenominator = (BigInt(this.denominator) * BigInt(other.denominator)).toLong
        NonNegativeInterval.reduce(newNumerator, newDenominator)
    }

    /** Division operation.
      *
      * @param other
      *   The other NonNegativeInterval to divide by
      * @return
      *   The quotient as a reduced NonNegativeInterval
      * @throws IllegalArgumentException
      *   if dividing by zero (other.numerator == 0)
      */
    def /(other: NonNegativeInterval): NonNegativeInterval = {
        require(other.numerator > 0, "Cannot divide by zero")
        val newNumerator = (BigInt(this.numerator) * BigInt(other.denominator)).toLong
        val newDenominator = (BigInt(this.denominator) * BigInt(other.numerator)).toLong
        NonNegativeInterval.reduce(newNumerator, newDenominator)
    }

    /** Less than comparison.
      *
      * @param other
      *   The other NonNegativeInterval to compare
      * @return
      *   true if this < other
      */
    def <(other: NonNegativeInterval): Boolean = {
        BigInt(this.numerator) * BigInt(other.denominator) < BigInt(other.numerator) * BigInt(
          this.denominator
        )
    }

    /** Less than or equal comparison.
      *
      * @param other
      *   The other NonNegativeInterval to compare
      * @return
      *   true if this <= other
      */
    def <=(other: NonNegativeInterval): Boolean = {
        BigInt(this.numerator) * BigInt(other.denominator) <= BigInt(other.numerator) * BigInt(
          this.denominator
        )
    }

    /** Greater than comparison.
      *
      * @param other
      *   The other NonNegativeInterval to compare
      * @return
      *   true if this > other
      */
    def >(other: NonNegativeInterval): Boolean = {
        BigInt(this.numerator) * BigInt(other.denominator) > BigInt(other.numerator) * BigInt(
          this.denominator
        )
    }

    /** Greater than or equal comparison.
      *
      * @param other
      *   The other NonNegativeInterval to compare
      * @return
      *   true if this >= other
      */
    def >=(other: NonNegativeInterval): Boolean = {
        BigInt(this.numerator) * BigInt(other.denominator) >= BigInt(other.numerator) * BigInt(
          this.denominator
        )
    }

    /** Ceiling operation - returns the smallest Long greater than or equal to this value.
      *
      * @return
      *   The ceiling as a Long
      */
    def ceil: Long = {
        if numerator % denominator == 0 then {
            numerator / denominator
        } else {
            (numerator / denominator) + 1
        }
    }

    /** Floor operation - returns the largest Long less than or equal to this value.
      *
      * @return
      *   The floor as a Long
      */
    def floor: Long = {
        numerator / denominator
    }

    def toBigDecimal: BigDecimal = BigDecimal(numerator) / BigDecimal(denominator)
    def toDouble: Double = toBigDecimal.toDouble
    def reduce: NonNegativeInterval = NonNegativeInterval.reduce(numerator, denominator)
}

object NonNegativeInterval {
    val zero: NonNegativeInterval = NonNegativeInterval(0, 1)

    def reduce(numerator: Long, denominator: Long): NonNegativeInterval = {
        val gcdValue = gcd(numerator, denominator)
        NonNegativeInterval(numerator / gcdValue, denominator / gcdValue)
    }

    def apply(numerator: Long): NonNegativeInterval = {
        NonNegativeInterval(numerator, 1)
    }

    /** Creates a NonNegativeInterval from a double value. Uses a precision of 1,000,000 for the
      * conversion.
      *
      * @param value
      *   A non-negative double
      * @return
      *   The corresponding NonNegativeInterval
      */
    def apply(value: Double, precision: Int = 6): NonNegativeInterval = {
        require(value >= 0, s"Value must be non-negative, got $value")
        require(precision >= 1 && precision <= 15, "Precision must be between 1 and 15")

        // Scale the double to an integer fraction
        val scale = math.pow(10, precision)
        val numerator = math.round(value * scale)
        val denominator = scale.toLong

        NonNegativeInterval(numerator, denominator)
    }

    @tailrec
    private def gcd(a: Long, b: Long): Long = {
        if b == 0 then a else gcd(b, a % b)
    }

    given UpickleReadWriter[NonNegativeInterval] =
        upickleReadwriter[Double].bimap[NonNegativeInterval](
          interval => interval.toDouble,
          double => NonNegativeInterval(double)
        )

    /** CBOR Encoder for NonNegativeInterval. Encodes as a tagged array [numerator, denominator]
      * with tag 30.
      */
    given Encoder[NonNegativeInterval] = new Encoder[NonNegativeInterval] {
        def write(w: Writer, value: NonNegativeInterval): Writer =
            w.writeTag(Tag.Other(30))
                .writeArrayOpen(2)
                .writeLong(value.numerator)
                .writeLong(value.denominator)
                .writeArrayClose()
    }

    /** CBOR Decoder for NonNegativeInterval. Decodes from a tagged array [numerator, denominator]
      * with tag 30.
      */
    given Decoder[NonNegativeInterval] = new Decoder[NonNegativeInterval] {
        def read(r: Reader): NonNegativeInterval = {
            r.readTag() // Read and discard tag 30
            r.readArrayHeader()
            val numerator = r.readLong()
            val denominator = r.readLong()
            NonNegativeInterval(numerator, denominator)
        }
    }
}
