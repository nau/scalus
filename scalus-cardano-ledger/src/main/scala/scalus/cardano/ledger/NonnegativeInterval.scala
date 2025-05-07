package scalus.cardano.ledger

import io.bullet.borer.{Decoder, Encoder, Reader, Tag, Writer}

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
case class NonnegativeInterval(numerator: Long, denominator: Long) {
    // Validate constraints
    require(denominator > 0, "Denominator must be positive")
    require(numerator >= 0, "Numerator must be non-negative")

    /** Converts this interval to a double value.
      *
      * @return
      *   The double value representation
      */
    def toDouble: Double = numerator.toDouble / denominator.toDouble
}

object NonnegativeInterval {

    /** Creates a NonnegativeInterval from a double value. Uses a precision of 1,000,000 for the
      * conversion.
      *
      * @param value
      *   A non-negative double
      * @return
      *   The corresponding NonnegativeInterval
      */
    def fromDouble(value: Double): NonnegativeInterval = {
        require(value >= 0, s"Value must be non-negative, got $value")

        val precision = 1000000L
        val numerator = Math.round(value * precision)

        NonnegativeInterval(numerator, precision)
    }

    /** CBOR Encoder for NonnegativeInterval. Encodes as a tagged array [numerator, denominator]
      * with tag 30.
      */
    given Encoder[NonnegativeInterval] = new Encoder[NonnegativeInterval] {
        def write(w: Writer, value: NonnegativeInterval): Writer =
            w.writeTag(Tag.Other(30))
                .writeArrayOpen(2)
                .writeLong(value.numerator)
                .writeLong(value.denominator)
                .writeArrayClose()
    }

    /** CBOR Decoder for NonnegativeInterval. Decodes from a tagged array [numerator, denominator]
      * with tag 30.
      */
    given Decoder[NonnegativeInterval] = new Decoder[NonnegativeInterval] {
        def read(r: Reader): NonnegativeInterval = {
            r.readTag() // Read and discard tag 30
            r.readArrayHeader()
            val numerator = r.readLong()
            val denominator = r.readLong()
            NonnegativeInterval(numerator, denominator)
        }
    }
}
