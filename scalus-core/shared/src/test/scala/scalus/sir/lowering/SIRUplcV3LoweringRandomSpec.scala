package scalus.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.sir.*
import scalus.Compiler.compile
import scalus.uplc.Term
import scalus.uplc.Constant
import scalus.uplc.eval.PlutusVM
import scalus.uplc.eval.Result

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
        // println(sir.showHighlighted)
        val lowering = SirToUplcV3Lowering(sir)
        val term = lowering.lower()
        // println(term.showHighlighted)
        // println(term.evaluateDebug)
    }

    test("lowering simple enum") {
        val sir = compile {
            val bb = BB.E(BigInt(1), BigInt(2))
            bb match
                case BB.C       => BigInt(0)
                case BB.D(a)    => a
                case BB.E(a, b) => a + b
        }
        // println(sir.showHighlighted)

        val lowering = SirToUplcV3Lowering(sir)
        val term = lowering.lower()
        // println(term.showHighlighted)
        val result = term.evaluateDebug
        // println(result)
        assert(result.isSuccess)
        result match {
            case Result.Success(term, budget, costs, log) =>
                assert(term == Term.Const(Constant.Integer(BigInt(3))))
            case Result.Failure(err, budget, costs, log) =>
                fail(s"Lowering failed with error: $err, budget: $budget, costs: $costs, log: $log")
        }
    }

}
