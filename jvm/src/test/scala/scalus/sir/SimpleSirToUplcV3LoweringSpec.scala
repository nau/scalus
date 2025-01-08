package scalus.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.*
import scalus.builtin.given
import scalus.builtin.ByteString.*
import scalus.uplc.eval.PlutusVM

import scala.language.implicitConversions

class SimpleSirToUplcV3LoweringSpec extends AnyFunSuite {
    given PlutusVM = PlutusVM.makePlutusV3VM()

    case class Asdf(
        a: BigInt,
        b: ByteString,
        c: Boolean,
        d: String,
        u: Unit,
        e1: BLS12_381_G1_Element,
        e2: BLS12_381_G2_Element,
        bb: AA
    )
    enum AA:
        case BBB
        case CCC

    test("V3 lowering") {
        val sir =
            compile:
                val asdf = new Asdf(
                  123,
                  hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6",
                  false,
                  "asdf",
                  (),
                  Builtins
                      .bls12_381_G1_hashToGroup(ByteString.empty, ByteString.fromString("DST")),
                  Builtins.bls12_381_G2_hashToGroup(
                    ByteString.empty,
                    ByteString.fromString("DST")
                  ),
                  AA.BBB
                )
                //                asdf
                asdf.bb

        println(sir.showHighlighted)
        val lower = SimpleSirToUplcV3Lowering(sir)
        val term = lower.lower()
        println(term.showHighlighted)
        println(term.evaluateDebug)
    }
}
