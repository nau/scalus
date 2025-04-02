package scalus.ledger

import scala.util.Try

// BIP173
// https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki
object Bech32 {
    type Int5 = Byte
    final val CHARSET = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"
    final val CHARSET_MAP: Map[Char, Int5] = CHARSET.zipWithIndex.toMap.view.mapValues(_.toByte).toMap
    final val CHARSET_REVERSE_MAP: Map[Int5, Char] = CHARSET_MAP.map(_.swap)

    final val SEP = '1'

    private final val GENERATOR = Seq(0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3)

    final def polymod(values: Seq[Int5]): Int = {
        var chk = 1
        values.foreach { v =>
            val b = chk >>> 25
            chk = ((chk & 0x1ffffff) << 5) ^ v
            (0 until 5).foreach {
                case i if ((b >>> i) & 1) != 0 =>
                    chk = chk ^ GENERATOR(i)
                case _ => ()
            }
        }
        chk
    }

    final def hrpExpand(s: String): Seq[Int5] = {
        val b = Array.newBuilder[Int5]
        s.foreach { c =>
            b += (c.toInt >>> 5).toByte
        }
        b += 0.toByte
        s.foreach { c =>
            b += (c.toInt & 31).toByte
        }
        b.result()
    }

    final def verifyCheckSum(hrp: String, data: Seq[Int5]): Boolean =
        polymod(hrpExpand(hrp) ++ data) == 1

    final def createChecksum(hrp: String, data: Seq[Int5]): Seq[Int5] = {
        val values = hrpExpand(hrp) ++ data
        val poly = polymod(
          values ++ Seq(0.toByte, 0.toByte, 0.toByte, 0.toByte, 0.toByte, 0.toByte)
        ) ^ 1.toByte
        (0 to 5).map(i => ((poly >>> 5 * (5 - i)) & 31).toByte)
    }

    final def decode(bech32: String): Try[(String, Seq[Int5])] = Try {
        val l = bech32.length
        require(
          l >= 8 && l <= 90,
          s"Invalid Bech32: $bech32 (length $l). Valid length range: 8-90 characters."
        )
        require(
          bech32.forall(c => c.isLower || c.isDigit) || bech32.forall(c => c.isUpper || c.isDigit),
          s"Invalid Bech32: $bech32. Mixed case."
        )
        val sepPosition = bech32.lastIndexOf(SEP)
        require(sepPosition != -1, s"Invalid Bech32: $bech32. Missing separator $SEP.")
        val input = bech32.toLowerCase()
        val hrp = input.take(sepPosition)
        val data = input.drop(sepPosition + 1).map(CHARSET_MAP)
        require(hrp.length >= 1, s"Invalid Bech32: $bech32. Invalid hrp length ${hrp.length}.")
        require(data.length >= 6, s"Invalid Bech32: $bech32. Invalid data length ${data.length}.")
        require(verifyCheckSum(hrp, data), s"Invalid checksum for $bech32")
        (hrp, data.dropRight(6))
    }.recover {
        case _: java.util.NoSuchElementException =>
            throw new IllegalArgumentException(
              s"requirement failed: Invalid Bech32: $bech32. Invalid Character. Valid (both cases): ${CHARSET
                      .mkString("[", ",", "]")}"
            )
        case t =>
            throw new IllegalArgumentException(
              s"requirement failed: Invalid Bech32: $bech32. " + t.getMessage
            )
    }

    final def encode(hrp: String, data: Seq[Int5]): Try[String] = Try {
        require(hrp.length >= 1, s"Invalid hrp length ${hrp.length}.")
        hrp + SEP + (data ++ createChecksum(hrp, data)).map(CHARSET_REVERSE_MAP).mkString
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

    def to5Bit(input: Seq[Byte]): Seq[Int5] = {
        var buffer = 0L
        var count = 0
        val builder = Array.newBuilder[Byte]

        input.foreach(b => {
            buffer = (buffer << 8) | (b & 0xff)
            count = count + 8
            while (count >= 5) {
                builder += ((buffer >> (count - 5)) & 31).toByte
                count = count - 5
            }
        })
        if (count > 0) builder += ((buffer << (5 - count)) & 31).toByte
        builder.result()
    }

    def from5Bit(input: Seq[Int5]): Seq[Byte] = {
        var buffer = 0L
        var count = 0
        val builder = Array.newBuilder[Byte]

        input.foreach(b => {
            buffer = (buffer << 5) | (b & 31)
            count = count + 5
            while (count >= 8) {
                builder += ((buffer >> (count - 8)) & 0xff).toByte
                count = count - 8
            }
        })
        require(count <= 4)
        require((buffer & ((1 << count) - 1)) == 0)
        builder.result()
    }
}
