package scalus.sir

import org.scalatest.funsuite.AnyFunSuite

object SIRUplcV3LoweringSpec {

    case class AA(flag: Boolean, a: BigInt)

    enum BB:
        case C
        case D(a: BigInt)
        case E(a: BigInt, b: BigInt)

    
}

class SIRUplcV3LoweringSpec extends AnyFunSuite {
    
    test("lowering simple case class") {
        val sir = compile {
            val aa = AA(true, BigInt(123))
            aa.a
        }
        println(sir.showHighlighted)
        val lowering = SirToUplcV3Lowering(sir)
        lowering.lower()
        println(term.showHighlighted)
        println(term.evaluateDebug)
    }
    
    
}
