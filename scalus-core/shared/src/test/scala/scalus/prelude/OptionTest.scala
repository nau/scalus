package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.Compiler.compile
import scalus.*
import scalus.prelude.{*, given}
import scalus.sir.SIR
import scalus.uplc.Term
import scalus.uplc.TermDSL.given
import scalus.uplc.eval.PlutusVM
import scalus.uplc.test.ArbitraryInstances

import scala.language.implicitConversions

class OptionTest extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances {
    private given PlutusVM = PlutusVM.makePlutusV2VM()
    test("eq") {
        assert((prelude.Option.None: prelude.Option[String]) === prelude.Option.None)
        assert(prelude.Option.Some(BigInt(1)) === prelude.Option.Some(BigInt(1)))
        assert(prelude.Option.Some(BigInt(1)) !== prelude.Option.Some(BigInt(2)))
        assertEval(compile(prelude.Option.Some(true) === prelude.Option.None), false)
        assertEval(compile(prelude.Option.Some(true) === prelude.Option.Some(true)), true)
        assertEval(compile(prelude.Option.Some(true) !== prelude.Option.Some(true)), false)
    }

    private def assertEval(sir: SIR, expected: Term) = {
        val term = sir.toUplc()
        assert(term.evaluate == expected)
    }
}
