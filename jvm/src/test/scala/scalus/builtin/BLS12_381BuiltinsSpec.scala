package scalus.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString.given

import scala.language.implicitConversions

// TODO: move to shared when BLS12-381 builtins on JS are implemented
class BLS12_381BuiltinsSpec extends AnyFunSuite {

    test("uncompress zero G1") {
        assert(
          bls12_381_G1_uncompress(
            bls12_381_G1_compressed_zero
          ).compressedByteString == hex"c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
        )
    }

    test("add zeros is zero in G1") {
        val zero = bls12_381_G1_uncompress(bls12_381_G1_compressed_zero)
        val sumZero = bls12_381_G1_add(zero, zero)
        assert(bls12_381_G1_equal(zero, sumZero))
    }

    test("add anythig and zero is zero in G1") {
        val zero = bls12_381_G1_uncompress(bls12_381_G1_compressed_zero)
        val g1 = bls12_381_G1_uncompress(bls12_381_G1_compressed_generator)
        val sumZero = bls12_381_G1_add(zero, g1)
        assert(bls12_381_G1_equal(zero, sumZero))
    }

    test("uncompress zero G2") {
        assert(
          bls12_381_G2_uncompress(
            bls12_381_G2_compressed_zero
          ).compressedByteString == hex"c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
        )
    }

    test("add zeros is zero in G2") {
        val zero = bls12_381_G2_uncompress(bls12_381_G2_compressed_zero)
        val sumZero = bls12_381_G2_add(zero, zero)
        assert(bls12_381_G2_equal(zero, sumZero))
    }

    test("add anythig and zero is zero in G2") {
        val zero = bls12_381_G2_uncompress(bls12_381_G2_compressed_zero)
        val g2 = bls12_381_G2_uncompress(bls12_381_G2_compressed_generator)
        val sumZero = bls12_381_G2_add(zero, g2)
        assert(bls12_381_G2_equal(zero, sumZero))
    }
}
