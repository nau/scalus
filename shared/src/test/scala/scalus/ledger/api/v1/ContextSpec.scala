package scalus.ledger.api.v1

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.Compiler.compile
import scalus.builtins.ByteString
import scalus.builtins.ByteString.given
import scalus.prelude.Prelude.*
import scalus.prelude.Prelude.given
import scalus.sir.SIR
import scalus.sir.SimpleSirToUplcLowering
import scalus.uplc.ArbitraryInstances
import scalus.uplc.Cek
import scalus.uplc.Term
import scalus.uplc.TermDSL.given
import scalus.uplc.Data
import scalus.uplc.Data.fromData
import scalus.uplc.Data.toData
import scalus.uplc.Constant
import scalus.pretty

class ContextSpec
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with ArbitraryInstances
    with scalus.ledger.api.v1.ArbitraryInstances {
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

  test("Interval Eq") {
    import scalus.sir.SirDSL.*
    val sir = compile { (d: Data) =>
      import scalus.uplc.FromDataInstances.given
      import scalus.ledger.api.v1.FromDataInstances.given
      val i = fromData[Interval[POSIXTime]](d)
      i === i
    }

    forAll { (i: Interval[POSIXTime]) =>
      import scalus.uplc.ToDataInstances.given
      import scalus.ledger.api.v1.ToDataInstances.given
      assert(i === i)
      val d = i.toData
      val applied = sir $ SIR.Const(Constant.Data(d))
      assertEval(applied, true)
    }
  }

  private def assertEval(sir: SIR, expected: Term) = {
    val term = new SimpleSirToUplcLowering().lower(sir)
    assert(Cek.evalUPLC(term) == expected)
  }
}
