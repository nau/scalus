package scalus.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.sir.SIRType
import scalus.sir.lowering.*
import scalus.sir.lowering.typegens.*

object SIRLoweringRepresentationTest {

    case class TestCaseClass(a: BigInt, b: String)

    enum TestSum:
        case CaseA(a: BigInt)
        case CaseB(b: String)
        case CaseC(c: Boolean)

    case class TestCaseClassOneElement(a: BigInt)

}

class SIRLoweringRepresentationTest extends AnyFunSuite {

    test("Get default representation for List") {
        val listType = SIRType.List(SIRType.Integer)
        given LoweringContext = LoweringContext()
        val generator = SirTypeUplcGenerator(listType)
        println(s"generator = ${generator} ")
        assert(generator.defaultRepresentation(listType) == SumCaseClassRepresentation.SumDataList)
    }

    test("Get default representation for Case Class") {
        val listType = SIRType.List(SIRType.Integer)
        given LoweringContext = LoweringContext()
        val generator = SirTypeUplcGenerator(listType)
        println(s"generator = ${generator} ")
        assert(generator.defaultRepresentation(listType) == SumCaseClassRepresentation.SumDataList)
    }

}
