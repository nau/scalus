package scalus.prelude.crypto.bls12_381

import org.scalatest.funsuite.AnyFunSuiteLike
import scalus.builtin.{Builtins, ByteString, PlatformSpecific}
import scalus.builtin.Builtins.*
import scalus.builtin.PlatformSpecific.{bls12_381_G1_compressed_generator, bls12_381_G1_compressed_zero}
import scalus.prelude.crypto.bls12_381.G1.{*, given}
import scalus.prelude.*
import scalus.uplc.eval.PlutusVM
import scalus.uplc.{Constant, Term}

class BLS12_381_G1Test extends AnyFunSuiteLike {

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    test("zero") {
        assert(bls12_381_G1_equal(zero, bls12_381_G1_uncompress(bls12_381_G1_compressed_zero)))
    }

    test("generator") {
        assert(
          bls12_381_G1_equal(generator, bls12_381_G1_uncompress(G1.generatorCompressed))
        )
    }

    test("equal") {
        assertEval(zero equal zero)
        assertEval(generator equal generator)
        assertEval(!(generator equal zero))
        assertEval(zero === zero)
        assertEval(zero !== generator)
    }

    test("add") {
        assertEval {
            generator + generator === Builtins.bls12_381_G1_add(generator, generator)
        }
    }

    test("scalar multiplication") {
        assertEval {
            generator.scale(2) === bls12_381_G1_scalarMul(2, generator)
        }
    }

    test("negation") {
        assertEval {
            generator + -generator === zero
        }
    }

    test("compress and uncompress") {
        assertEval {
            uncompress(generator.compress) === generator
        }
    }

    test("hash to group") {
        assertEval {
            val input = ByteString.fromHex("deadbeef")
            val dst = ByteString.fromHex("123456")
            val hashed = bls12_381_G1_hashToGroup(input, dst)
            // Ensure the result is not the zero element
            (hashed !== zero) && hashToGroup(input, dst) === hashed
        }
    }

    private inline def assertEval(inline code: Boolean): Unit = {
        import scalus.*
        assert(code)
        val term = Compiler.compileInline(code).toUplc(true).evaluate
        term match
            case Term.Const(Constant.Bool(b)) =>
                assert(b)
            case _ => fail(s"Unexpected term: $term")
    }
}
