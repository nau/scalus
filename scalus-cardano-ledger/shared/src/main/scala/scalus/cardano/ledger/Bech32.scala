package scalus.cardano.ledger

object Bech32 {
    type Int5 = Byte

    /** Result of decoding a Bech32 string. */
    final case class Bech32Decoded(hrp: String, data: Array[Byte])

    private final val CHARSET = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

    private final val CHARSET_MAP: Map[Char, Int5] = {
        var m = Map.empty[Char, Int5]
        var i = 0
        while i < CHARSET.length do {
            m += CHARSET.charAt(i) -> i.toByte
            i += 1
        }
        m
    }

    private final val CHARSET_REVERSE_MAP: Map[Int5, Char] = {
        var m = Map.empty[Int5, Char]
        val iter = CHARSET_MAP.iterator
        while iter.hasNext do {
            val (c, b) = iter.next()
            m += b -> c
        }
        m
    }

    final val SEP = '1'
    private final val GENERATOR = Array(0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3)

    /** Compute the Bech32 polymod checksum */
    final def polymod(values: Array[Int5]): Int = {
        var chk = 1
        var idx = 0
        while idx < values.length do {
            val v = values(idx)
            val b = chk >>> 25
            chk = ((chk & 0x1ffffff) << 5) ^ v
            var i = 0
            while i < 5 do {
                if ((b >>> i) & 1) != 0 then chk ^= GENERATOR(i)
                i += 1
            }
            idx += 1
        }
        chk
    }

    /** Expand the human-readable prefix for checksum calculation */
    final def hrpExpand(s: String): Array[Int5] = {
        val result = new Array[Int5](s.length * 2 + 1)
        var i = 0
        while i < s.length do {
            result(i) = (s.charAt(i) >>> 5).toByte
            i += 1
        }
        result(i) = 0.toByte
        i += 1
        var j = 0
        while j < s.length do {
            result(i + j) = (s.charAt(j) & 31).toByte
            j += 1
        }
        result
    }

    /** Verify the checksum of a Bech32 string */
    final def verifyCheckSum(hrp: String, data: Array[Int5]): Boolean = {
        val expanded = hrpExpand(hrp)
        val combined = new Array[Int5](expanded.length + data.length)
        System.arraycopy(expanded, 0, combined, 0, expanded.length)
        System.arraycopy(data, 0, combined, expanded.length, data.length)
        polymod(combined) == 1
    }

    /** Create the checksum bytes for Bech32 encoding */
    final def createChecksum(hrp: String, data: Array[Int5]): Array[Int5] = {
        val expanded = hrpExpand(hrp)
        val values = new Array[Int5](expanded.length + data.length + 6)
        System.arraycopy(expanded, 0, values, 0, expanded.length)
        System.arraycopy(data, 0, values, expanded.length, data.length)

        var i = 0
        while i < 6 do {
            values(expanded.length + data.length + i) = 0.toByte
            i += 1
        }

        val poly = polymod(values) ^ 1
        val result = new Array[Int5](6)
        i = 0
        while i < 6 do {
            result(i) = ((poly >>> (5 * (5 - i))) & 31).toByte
            i += 1
        }
        result
    }

    /** Decode a Bech32 string into its HRP and data (as bytes). Throws if invalid. */
    final def decode(bech32: String): Bech32Decoded = {
        val (hrp, data5) = decodeTo5Bit(bech32)
        Bech32Decoded(hrp, from5Bit(data5))
    }

    /** Decode a Bech32 string into HRP and 5-bit data. Throws if invalid. */
    final def decodeTo5Bit(bech32: String): (String, Array[Int5]) = {
        val sepPosition = bech32.lastIndexOf(SEP)
        require(sepPosition != -1, s"Invalid Bech32: missing separator '$SEP'.")

        val dataPart = bech32.substring(sepPosition + 1)
        require(
          dataPart.forall(c => c.isLower || c.isDigit) || dataPart.forall(c =>
              c.isUpper || c.isDigit
          ),
          s"Invalid Bech32: mixed case in data part."
        )

        val input = bech32.toLowerCase
        val hrp = input.take(sepPosition)
        val data = new Array[Int5](input.length - sepPosition - 1)

        var i = 0
        while i < data.length do {
            val c = input(sepPosition + 1 + i)
            data(i) = CHARSET_MAP.getOrElse(
              c,
              throw new IllegalArgumentException(
                s"Invalid character '$c'. Valid: ${CHARSET.mkString("[", ",", "]")}"
              )
            )
            i += 1
        }

        require(hrp.nonEmpty, "HRP must not be empty.")
        require(data.length >= 6, "Data too short for checksum.")
        require(verifyCheckSum(hrp, data), "Invalid checksum.")

        val dataWithoutChecksum = new Array[Int5](data.length - 6)
        System.arraycopy(data, 0, dataWithoutChecksum, 0, data.length - 6)
        (hrp, dataWithoutChecksum)
    }

    /** Encode an HRP and byte array into a Bech32 string. Throws if invalid. */
    final def encode(hrp: String, data: Array[Byte]): String = {
        encodeFrom5Bit(hrp, to5Bit(data))
    }

    /** Encode an HRP and 5-bit data into a Bech32 string. Throws if invalid. */
    final def encodeFrom5Bit(hrp: String, data: Array[Int5]): String = {
        require(hrp.nonEmpty, "HRP must not be empty.")

        val checksum = createChecksum(hrp, data)
        val combined = new Array[Int5](data.length + checksum.length)
        System.arraycopy(data, 0, combined, 0, data.length)
        System.arraycopy(checksum, 0, combined, data.length, checksum.length)

        val sb = new StringBuilder(hrp.length + 1 + combined.length)
        sb.append(hrp).append(SEP)

        var i = 0
        while i < combined.length do {
            val c = CHARSET_REVERSE_MAP.getOrElse(
              combined(i),
              throw new IllegalArgumentException("Invalid UInt5 value in data.")
            )
            sb.append(c)
            i += 1
        }
        sb.toString
    }

    /** Convert 8-bit bytes into 5-bit values. */
    final def to5Bit(input: Array[Byte]): Array[Int5] = {
        val outputSize = (input.length * 8 + 4) / 5
        val result = new Array[Int5](outputSize)

        var buffer = 0L
        var count = 0
        var outputIndex = 0

        var i = 0
        while i < input.length do {
            buffer = (buffer << 8) | (input(i) & 0xff)
            count += 8
            while count >= 5 do {
                result(outputIndex) = ((buffer >> (count - 5)) & 31).toByte
                count -= 5
                outputIndex += 1
            }
            i += 1
        }

        if count > 0 then {
            result(outputIndex) = ((buffer << (5 - count)) & 31).toByte
        }

        result
    }

    /** Convert 5-bit values into 8-bit bytes. Throws if invalid padding. */
    final def from5Bit(input: Array[Int5]): Array[Byte] = {
        val outputSize = input.length * 5 / 8
        val result = new Array[Byte](outputSize)

        var buffer = 0L
        var count = 0
        var outputIndex = 0

        var i = 0
        while i < input.length do {
            buffer = (buffer << 5) | (input(i) & 31)
            count += 5
            while count >= 8 do {
                result(outputIndex) = ((buffer >> (count - 8)) & 0xff).toByte
                count -= 8
                outputIndex += 1
            }
            i += 1
        }

        require(count <= 4, "Invalid padding in 5-bit encoding.")
        require((buffer & ((1 << count) - 1)) == 0, "Non-zero padding bits in 5-bit encoding.")

        result
    }
}
