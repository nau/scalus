package scalus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.sir.SIR
import scalus.uplc.ExprBuilder.compile
import scalus.uplc.{ArbitraryInstances, Constant, DefaultUni, ExprBuilder, NamedDeBruijn, Term}
import scalus.uplc.TermDSL.{lam, λ}

class CompileToSIRSpec extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances:
  test("compile literals") {
    assert(compile(false) == SIR.Const(Constant.Bool(false)))
    assert(compile(true) == SIR.Const(Constant.Bool(true)))
    assert(compile(()) == SIR.Const(Constant.Unit)) // FIXME
    assert(compile("foo") == SIR.Const(Constant.String("foo")))
  }
