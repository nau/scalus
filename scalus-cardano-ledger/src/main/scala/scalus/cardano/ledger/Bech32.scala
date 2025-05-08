package scalus.cardano.ledger

import scala.util.Try

// TODO: move from ledger to utils?

// BIP173
// https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki
object Bech32 {
    type Int5 = Byte
    final val CHARSET = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"
    final val CHARSET_MAP: Map[Char, Int5] =
        CHARSET.zipWithIndex.map((c, i) => c -> i.toByte).toMap
    final val CHARSET_REVERSE_MAP: Map[Int5, Char] =
        CHARSET_MAP.map((c, b) => b -> c)

    final val SEP = '1'

    private final val GENERATOR = Array(0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3)

    // Performs checksum calculation for bech32 encoding
    final def polymod(values: Array[Int5]): Int = {
        var chk = 1
        for v <- values do {
            val b = chk >>> 25
            chk = ((chk & 0x1ffffff) << 5) ^ v
            for i <- 0 until 5 do {
                if ((b >>> i) & 1) != 0 then {
                    chk = chk ^ GENERATOR(i)
                }
            }
        }
        chk
    }

    // Expands human-readable part for checksum computation
    final def hrpExpand(s: String): Array[Int5] = {
        val result = new Array[Int5](s.length * 2 + 1)
        var i = 0
        // First half: high bits
        while i < s.length do {
            result(i) = (s(i).toInt >>> 5).toByte
            i += 1
        }

        // Separator
        result(i) = 0.toByte
        i += 1

        // Second half: low bits
        var j = 0
        while j < s.length do {
            result(i + j) = (s(j).toInt & 31).toByte
            j += 1
        }

        result
    }

    // Verifies checksum of bech32 string
    final def verifyCheckSum(hrp: String, data: Array[Int5]): Boolean = {
        val expanded = hrpExpand(hrp)
        val combined = new Array[Int5](expanded.length + data.length)
        System.arraycopy(expanded, 0, combined, 0, expanded.length)
        System.arraycopy(data, 0, combined, expanded.length, data.length)
        polymod(combined) == 1
    }

    // Creates checksum for bech32 encoding
    final def createChecksum(hrp: String, data: Array[Int5]): Array[Int5] = {
        val expanded = hrpExpand(hrp)
        val values = new Array[Int5](expanded.length + data.length + 6)
        System.arraycopy(expanded, 0, values, 0, expanded.length)
        System.arraycopy(data, 0, values, expanded.length, data.length)
        // Add 6 zeros for checksum calculation
        for i <- 0 until 6 do {
            values(expanded.length + data.length + i) = 0.toByte
        }

        val poly = polymod(values) ^ 1
        val result = new Array[Int5](6)

        for i <- 0 to 5 do {
            result(i) = ((poly >>> 5 * (5 - i)) & 31).toByte
        }

        result
    }

    // Decodes bech32 string to hrp and byte array
    final def decode(bech32: String): Try[(String, Array[Byte])] = {
        decodeTo5Bit(bech32).map((h, bs) => (h, from5Bit(bs)))
    }

    // Decodes bech32 string to hrp and 5-bit array
    final def decodeTo5Bit(bech32: String): Try[(String, Array[Int5])] = Try {
        // Check for consistent case in the data part only (after separator)
        // The HRP part (before separator) can contain special chars like underscore
        val sepPosition = bech32.lastIndexOf(SEP)
        require(sepPosition != -1, s"Invalid Bech32: $bech32. Missing separator $SEP.")

        val dataPart = bech32.substring(sepPosition + 1)
        require(
          dataPart.forall(c => c.isLower || c.isDigit) || dataPart.forall(c =>
              c.isUpper || c.isDigit
          ),
          s"Invalid Bech32: $bech32. Mixed case in data part."
        )
        val input = bech32.toLowerCase()
        val hrp = input.take(sepPosition)
        val data = new Array[Int5](input.length - sepPosition - 1)

        var i = 0
        while i < data.length do {
            val c = input(sepPosition + 1 + i)
            data(i) = CHARSET_MAP.getOrElse(
              c,
              throw new IllegalArgumentException(
                s"Invalid Bech32: $bech32. Invalid Character. Valid (both cases): ${CHARSET.mkString("[", ",", "]")}"
              )
            )
            i += 1
        }

        require(hrp.length >= 1, s"Invalid Bech32: $bech32. Invalid hrp length ${hrp.length}.")
        require(data.length >= 6, s"Invalid Bech32: $bech32. Invalid data length ${data.length}.")
        require(verifyCheckSum(hrp, data), s"Invalid checksum for $bech32")

        val dataWithoutChecksum = new Array[Int5](data.length - 6)
        System.arraycopy(data, 0, dataWithoutChecksum, 0, data.length - 6)
        (hrp, dataWithoutChecksum)
    }.recover {
        case _: java.util.NoSuchElementException =>
            throw new IllegalArgumentException(
              s"requirement failed: Invalid Bech32: $bech32. Invalid Character. Valid (both cases): ${CHARSET.mkString("[", ",", "]")}"
            )
        case t =>
            throw new IllegalArgumentException(
              s"requirement failed: Invalid Bech32: $bech32. " + t.getMessage
            )
    }

    // Encodes hrp and byte array to bech32 string
    final def encode(hrp: String, data: Array[Byte]): Try[String] = {
        encodeFrom5Bit(hrp, to5Bit(data))
    }

    // Encodes hrp and 5-bit array to bech32 string
    final def encodeFrom5Bit(hrp: String, data: Array[Int5]): Try[String] = Try {
        require(hrp.length >= 1, s"Invalid hrp length ${hrp.length}.")

        val checksum = createChecksum(hrp, data)
        val combined = new Array[Int5](data.length + checksum.length)
        System.arraycopy(data, 0, combined, 0, data.length)
        System.arraycopy(checksum, 0, combined, data.length, checksum.length)

        val sb = new StringBuilder(hrp.length + 1 + combined.length)
        sb.append(hrp)
        sb.append(SEP)

        for b <- combined do {
            sb.append(
              CHARSET_REVERSE_MAP.getOrElse(
                b,
                throw new IllegalArgumentException(
                  s"requirement failed: Invalid data: $data. Valid data should contain only UInt5 values."
                )
              )
            )
        }

        sb.toString
    }.recover {
        case _: java.util.NoSuchElementException =>
            throw new IllegalArgumentException(
              s"requirement failed: Invalid data: $data. Valid data should contain only UInt5 values."
            )
        case t =>
            throw new IllegalArgumentException(
              s"requirement failed: Invalid hrp: $hrp or data: $data. " + t.getMessage
            )
    }

    // Converts 8-bit bytes to 5-bit bytes
    def to5Bit(input: Array[Byte]): Array[Int5] = {
        // Calculate output size (ceiling of input.length * 8 / 5)
        val outputSize = (input.length * 8 + 4) / 5
        val result = new Array[Int5](outputSize)

        var buffer = 0L
        var count = 0
        var outputIndex = 0

        for b <- input do {
            // Add 8 bits to buffer
            buffer = (buffer << 8) | (b & 0xff)
            count += 8

            // Extract 5-bit chunks while we have enough bits
            while count >= 5 do {
                result(outputIndex) = ((buffer >> (count - 5)) & 31).toByte
                count -= 5
                outputIndex += 1
            }
        }

        // Handle remaining bits if any
        if count > 0 then {
            result(outputIndex) = ((buffer << (5 - count)) & 31).toByte
        }

        result
    }

    // Converts 5-bit bytes to 8-bit bytes
    def from5Bit(input: Array[Int5]): Array[Byte] = {
        // Calculate output size (truncated to input.length * 5 / 8)
        val outputSize = input.length * 5 / 8
        val result = new Array[Byte](outputSize)

        var buffer = 0L
        var count = 0
        var outputIndex = 0

        for b <- input do {
            // Add 5 bits to buffer
            buffer = (buffer << 5) | (b & 31)
            count += 5

            // Extract 8-bit chunks when we have enough bits
            while count >= 8 do {
                result(outputIndex) = ((buffer >> (count - 8)) & 0xff).toByte
                count -= 8
                outputIndex += 1
            }
        }

        // Verify no remainder or incorrect padding
        require(count <= 4, "Invalid padding in 5-bit encoding")
        require((buffer & ((1 << count) - 1)) == 0, "Non-zero padding bits in 5-bit encoding")

        result
    }
}
