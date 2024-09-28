package scalus
package uplc
package eval

import scala.io.Source.fromFile
import scala.language.implicitConversions
import scala.util.Using

/** Tests for the Plutus Conformance Test Suite.
  *
  * @note
  *   This tests run only on JVM right now.
  */
class PlutusConformanceJvmSpec extends PlutusConformanceSpec:
    override protected val path = s"../${super.path}"
    protected def readFile(path: String): String = {
        Using.resource(scala.io.Source.fromFile(path))(_.mkString)
    }
    // TODO: for now, the BLS12-381 builtins implemented only for JVM
    // TODO: move to PlutusConformanceSpec when the BLS12-381 builtins are implemented for Scala.js
    // format: off
    check("builtin/constant/bls12-381/G1/bad-syntax-1/bad-syntax-1")
    check("builtin/constant/bls12-381/G1/bad-syntax-2/bad-syntax-2")
    check("builtin/constant/bls12-381/G1/bad-zero-1/bad-zero-1")
    check("builtin/constant/bls12-381/G1/bad-zero-2/bad-zero-2")
    check("builtin/constant/bls12-381/G1/bad-zero-3/bad-zero-3")
    check("builtin/constant/bls12-381/G1/off-curve/off-curve")
    check("builtin/constant/bls12-381/G1/on-curve-bit3-clear/on-curve-bit3-clear")
    check("builtin/constant/bls12-381/G1/on-curve-bit3-set/on-curve-bit3-set")
    check("builtin/constant/bls12-381/G1/on-curve-serialised-not-compressed/on-curve-serialised-not-compressed")
    check("builtin/constant/bls12-381/G1/out-of-group/out-of-group")
    check("builtin/constant/bls12-381/G1/too-long/too-long")
    check("builtin/constant/bls12-381/G1/too-short/too-short")
    check("builtin/constant/bls12-381/G1/zero/zero")
    check("builtin/constant/bls12-381/G2/bad-syntax-1/bad-syntax-1")
    check("builtin/constant/bls12-381/G2/bad-syntax-2/bad-syntax-2")
    check("builtin/constant/bls12-381/G2/bad-zero-1/bad-zero-1")
    check("builtin/constant/bls12-381/G2/bad-zero-2/bad-zero-2")
    check("builtin/constant/bls12-381/G2/bad-zero-3/bad-zero-3")
    check("builtin/constant/bls12-381/G2/off-curve/off-curve")
    check("builtin/constant/bls12-381/G2/on-curve-bit3-clear/on-curve-bit3-clear")
    check("builtin/constant/bls12-381/G2/on-curve-bit3-set/on-curve-bit3-set")
    check("builtin/constant/bls12-381/G2/on-curve-serialised-not-compressed/on-curve-serialised-not-compressed")
    check("builtin/constant/bls12-381/G2/out-of-group/out-of-group")
    check("builtin/constant/bls12-381/G2/too-long/too-long")
    check("builtin/constant/bls12-381/G2/too-short/too-short")
    check("builtin/constant/bls12-381/G2/zero/zero")
    check("builtin/semantics/bls12_381-cardano-crypto-tests/G1/arith/add/add")
    check("builtin/semantics/bls12_381-cardano-crypto-tests/G1/arith/neg/neg")
    check("builtin/semantics/bls12_381-cardano-crypto-tests/G1/arith/scalarMul/scalarMul")
    check("builtin/semantics/bls12_381-cardano-crypto-tests/G1/uncompress/off-curve/off-curve")
    check("builtin/semantics/bls12_381-cardano-crypto-tests/G1/uncompress/out-of-group/out-of-group")
    check("builtin/semantics/bls12_381-cardano-crypto-tests/G2/arith/add/add")
    check("builtin/semantics/bls12_381-cardano-crypto-tests/G2/arith/neg/neg")
    check("builtin/semantics/bls12_381-cardano-crypto-tests/G2/arith/scalarMul/scalarMul")
    check("builtin/semantics/bls12_381-cardano-crypto-tests/G2/uncompress/off-curve/off-curve")
    check("builtin/semantics/bls12_381-cardano-crypto-tests/G2/uncompress/out-of-group/out-of-group")
    check("builtin/semantics/bls12_381-cardano-crypto-tests/pairing/balanced/balanced")
    check("builtin/semantics/bls12_381-cardano-crypto-tests/pairing/left-additive/left-additive")
    check("builtin/semantics/bls12_381-cardano-crypto-tests/pairing/left-multiplicative/left-multiplicative")
    check("builtin/semantics/bls12_381-cardano-crypto-tests/pairing/right-additive/right-additive")
    check("builtin/semantics/bls12_381-cardano-crypto-tests/pairing/right-multiplicative/right-multiplicative")
    check("builtin/semantics/bls12_381-cardano-crypto-tests/pairing/swap-scalars/swap-scalars")
    check("builtin/semantics/bls12_381-cardano-crypto-tests/signature/augmented/augmented")
    // FIXME: This test is failing because blst Java binding for hash_to receive a String for DST
    //  and then convert it to a byte array as UTF-8. This is a bug in the Java bindings.
    // Here is the discussion: https://github.com/supranational/blst/pull/232
    // For now, we are skipping this test.
    //    check("builtin/semantics/bls12_381-cardano-crypto-tests/signature/large-dst/large-dst")
    check("builtin/semantics/bls12_381_G1_add/add-associative/add-associative")
    check("builtin/semantics/bls12_381_G1_add/add-commutative/add-commutative")
    check("builtin/semantics/bls12_381_G1_add/add-zero/add-zero")
    check("builtin/semantics/bls12_381_G1_add/add/add")
    check("builtin/semantics/bls12_381_G1_compress/compress/compress")
    check("builtin/semantics/bls12_381_G1_equal/equal-false/equal-false")
    check("builtin/semantics/bls12_381_G1_equal/equal-true/equal-true")
    check("builtin/semantics/bls12_381_G1_hashToGroup/hash-different-msg-same-dst/hash-different-msg-same-dst")
    // FIXME: This test is failing because blst Java binding for hash_to receive a String for DST
    //  and then convert it to a byte array as UTF-8. This is a bug in the Java bindings.
    // Here is the discussion: https://github.com/supranational/blst/pull/232
    // For now, we are skipping this test.
    //    check("builtin/semantics/bls12_381_G1_hashToGroup/hash-dst-len-255/hash-dst-len-255")
    check("builtin/semantics/bls12_381_G1_hashToGroup/hash-dst-len-256/hash-dst-len-256")
    check("builtin/semantics/bls12_381_G1_hashToGroup/hash-empty-dst/hash-empty-dst")
    check("builtin/semantics/bls12_381_G1_hashToGroup/hash-same-msg-different-dst/hash-same-msg-different-dst")
    check("builtin/semantics/bls12_381_G1_hashToGroup/hash/hash")
    check("builtin/semantics/bls12_381_G1_neg/add-neg/add-neg")
    check("builtin/semantics/bls12_381_G1_neg/neg-zero/neg-zero")
    check("builtin/semantics/bls12_381_G1_neg/neg/neg")
    check("builtin/semantics/bls12_381_G1_scalarMul/addmul/addmul")
    check("builtin/semantics/bls12_381_G1_scalarMul/mul0/mul0")
    check("builtin/semantics/bls12_381_G1_scalarMul/mul1/mul1")
    check("builtin/semantics/bls12_381_G1_scalarMul/mul19+25/mul19+25")
    check("builtin/semantics/bls12_381_G1_scalarMul/mul44/mul44")
    check("builtin/semantics/bls12_381_G1_scalarMul/mul4x11/mul4x11")
    check("builtin/semantics/bls12_381_G1_scalarMul/muladd/muladd")
    check("builtin/semantics/bls12_381_G1_scalarMul/mulneg1/mulneg1")
    check("builtin/semantics/bls12_381_G1_scalarMul/mulneg44/mulneg44")
    check("builtin/semantics/bls12_381_G1_scalarMul/mulperiodic1/mulperiodic1")
    check("builtin/semantics/bls12_381_G1_scalarMul/mulperiodic2/mulperiodic2")
    check("builtin/semantics/bls12_381_G1_scalarMul/mulperiodic3/mulperiodic3")
    check("builtin/semantics/bls12_381_G1_scalarMul/mulperiodic4/mulperiodic4")
    check("builtin/semantics/bls12_381_G1_uncompress/bad-zero-1/bad-zero-1")
    check("builtin/semantics/bls12_381_G1_uncompress/bad-zero-2/bad-zero-2")
    check("builtin/semantics/bls12_381_G1_uncompress/bad-zero-3/bad-zero-3")
    check("builtin/semantics/bls12_381_G1_uncompress/off-curve/off-curve")
    check("builtin/semantics/bls12_381_G1_uncompress/on-curve-bit1-clear/on-curve-bit1-clear")
    check("builtin/semantics/bls12_381_G1_uncompress/on-curve-bit3-clear/on-curve-bit3-clear")
    check("builtin/semantics/bls12_381_G1_uncompress/on-curve-bit3-set/on-curve-bit3-set")
    check("builtin/semantics/bls12_381_G1_uncompress/on-curve-serialised-not-compressed/on-curve-serialised-not-compressed")
    check("builtin/semantics/bls12_381_G1_uncompress/out-of-group/out-of-group")
    check("builtin/semantics/bls12_381_G1_uncompress/too-long/too-long")
    check("builtin/semantics/bls12_381_G1_uncompress/too-short/too-short")
    check("builtin/semantics/bls12_381_G1_uncompress/zero/zero")
    check("builtin/semantics/bls12_381_G2_add/add-associative/add-associative")
    check("builtin/semantics/bls12_381_G2_add/add-commutative/add-commutative")
    check("builtin/semantics/bls12_381_G2_add/add-zero/add-zero")
    check("builtin/semantics/bls12_381_G2_add/add/add")
    check("builtin/semantics/bls12_381_G2_compress/compress/compress")
    check("builtin/semantics/bls12_381_G2_equal/equal-false/equal-false")
    check("builtin/semantics/bls12_381_G2_equal/equal-true/equal-true")
    check("builtin/semantics/bls12_381_G2_hashToGroup/hash-different-msg-same-dst/hash-different-msg-same-dst")
    // FIXME: This test is failing because blst Java binding for hash_to receive a String for DST
    //  and then convert it to a byte array as UTF-8. This is a bug in the Java bindings.
    // Here is the discussion: https://github.com/supranational/blst/pull/232
    // For now, we are skipping this test.
    //    check("builtin/semantics/bls12_381_G2_hashToGroup/hash-dst-len-255/hash-dst-len-255")
    check("builtin/semantics/bls12_381_G2_hashToGroup/hash-dst-len-256/hash-dst-len-256")
    check("builtin/semantics/bls12_381_G2_hashToGroup/hash-empty-dst/hash-empty-dst")
    check("builtin/semantics/bls12_381_G2_hashToGroup/hash-same-msg-different-dst/hash-same-msg-different-dst")
    check("builtin/semantics/bls12_381_G2_hashToGroup/hash/hash")
    check("builtin/semantics/bls12_381_G2_neg/add-neg/add-neg")
    check("builtin/semantics/bls12_381_G2_neg/neg-zero/neg-zero")
    check("builtin/semantics/bls12_381_G2_neg/neg/neg")
    check("builtin/semantics/bls12_381_G2_scalarMul/addmul/addmul")
    check("builtin/semantics/bls12_381_G2_scalarMul/mul0/mul0")
    check("builtin/semantics/bls12_381_G2_scalarMul/mul1/mul1")
    check("builtin/semantics/bls12_381_G2_scalarMul/mul19+25/mul19+25")
    check("builtin/semantics/bls12_381_G2_scalarMul/mul44/mul44")
    check("builtin/semantics/bls12_381_G2_scalarMul/mul4x11/mul4x11")
    check("builtin/semantics/bls12_381_G2_scalarMul/muladd/muladd")
    check("builtin/semantics/bls12_381_G2_scalarMul/mulneg1/mulneg1")
    check("builtin/semantics/bls12_381_G2_scalarMul/mulneg44/mulneg44")
    check("builtin/semantics/bls12_381_G2_scalarMul/mulperiodic1/mulperiodic1")
    check("builtin/semantics/bls12_381_G2_scalarMul/mulperiodic2/mulperiodic2")
    check("builtin/semantics/bls12_381_G2_scalarMul/mulperiodic3/mulperiodic3")
    check("builtin/semantics/bls12_381_G2_scalarMul/mulperiodic4/mulperiodic4")
    check("builtin/semantics/bls12_381_G2_uncompress/bad-zero-1/bad-zero-1")
    check("builtin/semantics/bls12_381_G2_uncompress/bad-zero-2/bad-zero-2")
    check("builtin/semantics/bls12_381_G2_uncompress/bad-zero-3/bad-zero-3")
    check("builtin/semantics/bls12_381_G2_uncompress/off-curve/off-curve")
    check("builtin/semantics/bls12_381_G2_uncompress/on-curve-bit1-clear/on-curve-bit1-clear")
    check("builtin/semantics/bls12_381_G2_uncompress/on-curve-bit3-clear/on-curve-bit3-clear")
    check("builtin/semantics/bls12_381_G2_uncompress/on-curve-bit3-set/on-curve-bit3-set")
    check("builtin/semantics/bls12_381_G2_uncompress/on-curve-serialised-not-compressed/on-curve-serialised-not-compressed")
    check("builtin/semantics/bls12_381_G2_uncompress/out-of-group/out-of-group")
    check("builtin/semantics/bls12_381_G2_uncompress/too-long/too-long")
    check("builtin/semantics/bls12_381_G2_uncompress/too-short/too-short")
    check("builtin/semantics/bls12_381_G2_uncompress/zero/zero")
    check("builtin/semantics/bls12_381_millerLoop/balanced/balanced")
    check("builtin/semantics/bls12_381_millerLoop/equal-pairing/equal-pairing")
    check("builtin/semantics/bls12_381_millerLoop/left-additive/left-additive")
    check("builtin/semantics/bls12_381_millerLoop/random-pairing/random-pairing")
    check("builtin/semantics/bls12_381_millerLoop/right-additive/right-additive")
