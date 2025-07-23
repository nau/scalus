package scalus.prelude.crypto.bls12_381

import scalus.Compile
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString.hex
import scalus.builtin.{BLS12_381_G2_Element, ByteString}
import scalus.prelude.Eq

@Compile
object G2 {

    given Eq[BLS12_381_G2_Element] = bls12_381_G2_equal

    /** BLS12 G2 zero element.
      *
      * This is the point at infinity in the BLS12-381 G2 group
      *
      * @note
      *   This is a function, not a constant deliberately. Being a constant would make it always
      *   initialize in your Plutus script (which is not cheap) even if the BLS12-381 is not used in
      *   the evaluated code. Hence, to avoid multiple initializations, please create a local
      *   variable in the scope where you need it, like:
      *   {{{
      *     val zeroG2 = G2.zero
      *   }}}
      */
    def zero: BLS12_381_G2_Element = uncompress(
      hex"c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    )

    /** The compressed generator of the G2 group of the BLS12-381 curve.
      *
      * @note
      *   This is a function, not a constant deliberately. Being a constant would make it always
      *   initialize in your Plutus script (which is not cheap) even if the BLS12-381 is not used in
      *   the evaluated code. Hence, to avoid multiple initializations, please create a local
      *   variable in the scope where you need it, like:
      *   {{{
      *     val genG2 = G2.generator
      *   }}}
      */
    def generator: BLS12_381_G2_Element = uncompress(
      hex"93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8"
    )

    /** Uncompresses a point in the G2 group from its compressed form. */
    inline def uncompress(bs: ByteString): BLS12_381_G2_Element = {
        bls12_381_G2_uncompress(bs)
    }

    /** Hashes a [[ByteString]] to a point in the G2 group.
      *
      * @param bs
      *   The byte string to hash.
      * @param dst
      *   The domain separation tag, which should be a short byte string (up to 255 bytes).
      * @return
      *   A point in the G2 group.
      */
    inline def hashToGroup(bs: ByteString, dst: ByteString): BLS12_381_G2_Element = {
        bls12_381_G2_hashToGroup(bs, dst)
    }

    extension (self: BLS12_381_G2_Element) {

        /** Checks if two points in the G2 group are equal */
        inline def equal(rhs: BLS12_381_G2_Element): Boolean = bls12_381_G2_equal(self, rhs)

        /** Adds two points in the G2 group */
        infix inline def +(rhs: BLS12_381_G2_Element): BLS12_381_G2_Element =
            bls12_381_G2_add(self, rhs)

        /** Exponentiates a point in the G2 group with a `scalar`. This operation is equivalent to
          * the repeated addition of the point with itself `e` times.
          *
          * @param scalar
          *   The scalar to multiply the point by.
          * @return
          *   A new point in the G2 group, which is the result of multiplying the original point by
          *   the scalar.
          */
        inline def scale(scalar: BigInt): BLS12_381_G2_Element = {
            bls12_381_G2_scalarMul(scalar, self)
        }

        /** Negates the point in the G2 group */
        inline def unary_- : BLS12_381_G2_Element = {
            bls12_381_G2_neg(self)
        }

        /** Compresses the point in the G2 group to its compressed form.
          *
          * @return
          *   A [[ByteString]] representing the compressed point.
          */
        inline def compress: ByteString = bls12_381_G2_compress(self)
    }
}
