package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.Compiler.compile
import scalus.*
import scalus.builtin.given
import scalus.prelude.Maybe.*
import scalus.prelude.Prelude.{*, given}
import scalus.sir.SIR
import scalus.uplc.ArbitraryInstances
import scalus.uplc.Term
import scalus.uplc.TermDSL.given
import scalus.uplc.eval.PlutusVM

import scala.language.implicitConversions

class MaybeSpec extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances {
    private given PlutusVM = PlutusVM.makePlutusV2VM()
    test("eq") {
        assert((Nothing: Maybe[String]) === Nothing)
        assert(Just(BigInt(1)) === Just(BigInt(1)))
        assert(Just(BigInt(1)) !== Just(BigInt(2)))
        assertEval(compile(Just(true) === Nothing), false)
        assertEval(compile(Just(true) === Just(true)), true)
        assertEval(compile(Just(true) !== Just(true)), false)
    }

    private def assertEval(sir: SIR, expected: Term) = {
        val term = sir.toUplc()
        assert(term.evaluate == expected)
    }
}
