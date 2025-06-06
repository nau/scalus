package scalus.prelude.crypto.bls12_381
import scalus.Compile
import scalus.builtin.BLS12_381_G1_Element
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString.hex
import scalus.builtin.ByteString
import scalus.prelude.Eq

@Compile
object G1 {

    given Eq[BLS12_381_G1_Element] = bls12_381_G1_equal

    /** BLS12 G1 zero element.
      *
      * This is the point at infinity in the BLS12-381 G1 group
      */
    val zero: BLS12_381_G1_Element = uncompress(
      hex"c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    )

    /** The compressed generator of the G1 group of the BLS12-381 curve.
      */
    val generator: BLS12_381_G1_Element = uncompress(
      hex"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"
    )

    /** Uncompresses a point in the G1 group from its compressed form. */
    inline def uncompress(bs: ByteString): BLS12_381_G1_Element = {
        bls12_381_G1_uncompress(bs)
    }

    /** Hashes a [[ByteString]] to a point in the G1 group.
      *
      * @param bs
      *   The byte string to hash.
      * @param dst
      *   The domain separation tag, which should be a short byte string (up to 255 bytes).
      * @return
      *   A point in the G1 group.
      */
    inline def hashToGroup(bs: ByteString, dst: ByteString): BLS12_381_G1_Element = {
        bls12_381_G1_hashToGroup(bs, dst)
    }

    extension (self: BLS12_381_G1_Element) {

        /** Checks if two points in the G1 group are equal */
        inline def equal(rhs: BLS12_381_G1_Element): Boolean = bls12_381_G1_equal(self, rhs)

        /** Adds two points in the G1 group */
        infix inline def +(rhs: BLS12_381_G1_Element): BLS12_381_G1_Element =
            bls12_381_G1_add(self, rhs)

        /** Exponentiates a point in the G1 group with a `scalar`. This operation is equivalent to
          * the repeated addition of the point with itself `e` times.
          *
          * @param scalar
          *   The scalar to multiply the point by.
          * @return
          *   A new point in the G1 group, which is the result of multiplying the original point by
          *   the scalar.
          */
        inline def scale(scalar: BigInt): BLS12_381_G1_Element = {
            bls12_381_G1_scalarMul(scalar, self)
        }

        /** Negates the point in the G1 group */
        inline def unary_- : BLS12_381_G1_Element = {
            bls12_381_G1_neg(self)
        }

        /** Compresses the point in the G1 group to its compressed form.
          *
          * @return
          *   A [[ByteString]] representing the compressed point.
          */
        inline def compress: ByteString = bls12_381_G1_compress(self)
    }
}
