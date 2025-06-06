package scalus.prelude.crypto.bls12_381
import scalus.Compile
import scalus.builtin.BLS12_381_G1_Element
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString.hex
import scalus.builtin.{ByteString, PlatformSpecific}

@Compile
object G1 {

    /** BLS12 G1 zero element.
      *
      * This is the point at infinity in the BLS12-381 G1 group
      */
    val zero: BLS12_381_G1_Element = bls12_381_G1_uncompress(
      hex"c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    )

    val generator: BLS12_381_G1_Element = bls12_381_G1_uncompress(
      ByteString.fromHex(
        "97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"
      )
    )

    extension (self: BLS12_381_G1_Element) {
        infix def equal(rhs: BLS12_381_G1_Element): Boolean = bls12_381_G1_equal(self, rhs)

    }
}
