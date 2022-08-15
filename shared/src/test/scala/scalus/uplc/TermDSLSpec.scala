package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.uplc.TermDSL.{*, given}

class TermDSLSpec extends AnyFunSuite with ScalaCheckPropertyChecks:
  test("constant as Term") {
    val int: Term = BigInt(2)
    assert(int == Term.Const(Constant(DefaultUni.Integer, 2)))
    val bool: Term = true
    assert(bool == Term.Const(Constant(DefaultUni.Bool, true)))
  }
