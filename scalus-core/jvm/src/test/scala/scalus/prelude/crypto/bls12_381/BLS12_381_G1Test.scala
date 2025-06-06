package scalus.prelude.crypto.bls12_381

import org.scalatest.funsuite.AnyFunSuiteLike
import scalus.builtin.JVMPlatformSpecific.*
import scalus.builtin.PlatformSpecific.{bls12_381_G1_compressed_generator, bls12_381_G1_compressed_zero}
import scalus.builtin.{JVMPlatformSpecific, PlatformSpecific, given}
import scalus.prelude.crypto.bls12_381.G1.*
import scalus.uplc.eval.PlutusVM
import scalus.uplc.{Constant, Term}

class BLS12_381_G1Test extends AnyFunSuiteLike {

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    test("zero") {
        assert(bls12_381_G1_equal(G1.zero, bls12_381_G1_uncompress(bls12_381_G1_compressed_zero)))
    }

    test("generator") {
        assert(
          bls12_381_G1_equal(
            G1.generator,
            bls12_381_G1_uncompress(bls12_381_G1_compressed_generator)
          )
        )
    }

    test("equal") {
        assertEval(G1.zero equal G1.zero)
        assertEval(G1.generator equal G1.generator)
        assertEval(!(G1.generator equal G1.zero))
    }

    private inline def assertEval(inline code: Boolean): Unit = {
        import scalus.*
        assert(code)
        val term = Compiler.compileToUplc(code).evaluate
        term match
            case Term.Const(Constant.Bool(b)) =>
                assert(b)
            case _ => fail(s"Unexpected term: $term")
    }

}
