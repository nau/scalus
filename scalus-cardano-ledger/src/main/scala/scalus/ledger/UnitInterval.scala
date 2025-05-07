package scalus.ledger

import io.bullet.borer.{Decoder, Encoder, Reader, Tag, Writer}

/** Represents a unit interval (a number between 0 and 1) in the Cardano blockchain.
  *
  * A unit interval is represented as a fraction with a numerator and denominator, where numerator
  * <= denominator and denominator > 0.
  *
  * @param numerator
  *   The numerator of the fraction
  * @param denominator
  *   The denominator of the fraction
  */
case class UnitInterval(numerator: Long, denominator: Long) {
    // Validate constraints
    require(denominator > 0, "Denominator must be positive")
    require(numerator <= denominator, "Numerator must be less than or equal to denominator")
    require(numerator >= 0, "Numerator must be non-negative")

    /** Converts this unit interval to a double value.
      *
      * @return
      *   The double value representation
      */
    def toDouble: Double = numerator.toDouble / denominator.toDouble
}

object UnitInterval {

    /** Creates a UnitInterval from a double value. Uses a precision of 1,000,000 for the
      * conversion.
      *
      * @param value
      *   A double between 0 and 1
      * @return
      *   The corresponding UnitInterval
      */
    def fromDouble(value: Double): UnitInterval = {
        require(value >= 0 && value <= 1, s"Value must be between 0 and 1, got $value")

        val precision = 1000000L
        val numerator = Math.round(value * precision)

        UnitInterval(numerator, precision)
    }

    /** A predefined UnitInterval representing 0.5 (1/2).
      */
    val half: UnitInterval = UnitInterval(1, 2)

    /** A predefined UnitInterval representing 0 (0/1).
      */
    val zero: UnitInterval = UnitInterval(0, 1)

    /** A predefined UnitInterval representing 1 (1/1).
      */
    val one: UnitInterval = UnitInterval(1, 1)

    /** CBOR Encoder for UnitInterval. Encodes as a tagged array [numerator, denominator] with tag
      * 30.
      */
    given Encoder[UnitInterval] = new Encoder[UnitInterval] {
        def write(w: Writer, value: UnitInterval): Writer =
            w.writeTag(Tag.Other(30))
                .writeArrayOpen(2)
                .writeLong(value.numerator)
                .writeLong(value.denominator)
                .writeArrayClose()
    }

    /** CBOR Decoder for UnitInterval. Decodes from a tagged array [numerator, denominator] with tag
      * 30.
      */
    given Decoder[UnitInterval] = new Decoder[UnitInterval] {
        def read(r: Reader): UnitInterval = {
            r.readTag(Tag.Other(30)) // Read and discard tag 30
            r.readArrayHeader(2)
            val numerator = r.readLong()
            val denominator = r.readLong()
            UnitInterval(numerator, denominator)
        }
    }
}
