package scalus.ledger.api.v1

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.Compiler.compile
import scalus.builtins.ByteString
import scalus.builtins.ByteString.given
import scalus.prelude.Prelude.*
import scalus.sir.SIR
import scalus.sir.SimpleSirToUplcLowering
import scalus.uplc.ArbitraryInstances
import scalus.uplc.Cek
import scalus.uplc.Term
import scalus.uplc.TermDSL.given

class ContextSpec extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances {
  test("PubKeyHash Eq") {
    assert(PubKeyHash(ByteString.empty) === PubKeyHash(ByteString.empty))
    assert(PubKeyHash(hex"aa") !== PubKeyHash(ByteString.empty))
    assertEval(compile(new PubKeyHash(ByteString.empty) === new PubKeyHash(ByteString.empty)), true)
    assertEval(
      compile(new PubKeyHash(hex"aa") === new PubKeyHash(ByteString.empty)),
      false
    )
    assertEval(
      compile(
        new PubKeyHash(hex"aa") === new PubKeyHash(hex"aa")
      ),
      true
    )
  }

  private def assertEval(sir: SIR, expected: Term) = {
    val term = new SimpleSirToUplcLowering().lower(sir)
    assert(Cek.evalUPLC(term) == expected)
  }
}
