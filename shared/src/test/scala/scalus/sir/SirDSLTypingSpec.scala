package scalus.sir


import scalus.sir.SirDSL.{*, given}

import org.scalatest.funsuite.AnyFunSuite

class SirDSLTypingSpec extends AnyFunSuite:

    test("type list of ints moved to SIR via implicit conversion") {
        val sir: SIRExpr = List(1,2,3)
        if (sir.isInstanceOf[SIRExpr]) then
            val expr = sir.asInstanceOf[SIRExpr]
            assert(expr.tp == SIRType.List(SIRType.IntegerPrimitive))
        else
            fail("Expected SIRExpr")
    }
