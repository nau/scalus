package scalus.sir

import org.scalatest.funsuite.AnyFunSuite

import scalus.sir.lowering.*
import scalus.sir.lowering.typegens.*

class SIRLoweringRepresentationTest extends AnyFunSuite {

    test("Get default representation for List") {
        pending
        val listType = SIRType.List(SIRType.Integer)
        val generator = SirTypeUplcGenerator(listType)
        println(s"generator = ${generator} ")
        assert(generator.defaultRepresentation == SumCaseClassRepresentation.SumDataList)
    }

}
