package scalus.sir

import scalus.Compiler.compile
import scalus.*
import scalus.builtin.{*, given}
import scalus.uplc.eval.PlutusVM

import org.scalatest.funsuite.AnyFunSuite

object SIRUplcV3LoweringSpec {

    case class AA(flag: Boolean, a: BigInt)

    enum BB:
        case C
        case D(a: BigInt)
        case E(a: BigInt, b: BigInt)

}

class SIRUplcV3LoweringSpec extends AnyFunSuite {

    import SIRUplcV3LoweringSpec.*

    given PlutusVM = PlutusVM.makePlutusV3VM()

    test("lowering simple case class") {
        val sir = compile {
            val aa = AA(true, BigInt(123))
            aa.a
        }
        println(sir.showHighlighted)
        val lowering = SirToUplcV3Lowering(sir)
        val term = lowering.lower()
        println(term.showHighlighted)
        println(term.evaluateDebug)
    }

}
