package scalus.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.sir.*
import scalus.builtin.{Data, FromData, ToData}
import scalus.Compiler.compile
import scalus.uplc.Term
import scalus.uplc.Constant
import scalus.uplc.eval.PlutusVM
import scalus.uplc.eval.Result

object SIRUplcV3LoweringRandomSpec {

    case class AA(flag: Boolean, a: BigInt) derives FromData, ToData

    enum BB derives FromData, ToData:
        case C
        case D(a: BigInt)
        case E(a: BigInt, b: BigInt)

}

inline given scalus.Compiler.Options = scalus.Compiler.Options(
  targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
  generateErrorTraces = true,
  optimizeUplc = true,
  debug = true
)

class SIRUplcV3LoweringRandomSpec extends AnyFunSuite {

    import SIRUplcV3LoweringRandomSpec.*

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

    test("lowering match on simple case class") {
        val sir = compile { (data: Data) =>
            // val aa = AA(true, BigInt(123))
            val aa = Data.fromData[AA](data)
            aa match {
                case AA(flag, a) =>
                    if a != BigInt(123) then {
                        scalus.prelude.fail("Expected a to be 123")
                    }
            }
        }
        // println(sir.showHighlighted)
        val lowering = SirToUplcV3Lowering(sir)
        val term = lowering.lower()
        val aa = AA(true, BigInt(123))
        val termWithData = term $ Term.Const(Constant.Data(Data.toData(aa)))
        println(term.showHighlighted)
        val result = termWithData.evaluateDebug
        assert(result.isSuccess, s"Lowere dcode failed with result: $result")
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
