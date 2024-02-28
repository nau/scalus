package scalus.ledger.api.v1

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.ByteString.given
import scalus.builtin.Data.{fromData, toData}
import scalus.builtin.{ByteString, Data, given}
import scalus.prelude.Prelude.{*, given}
import scalus.sir.SIR
import scalus.uplc.{ArbitraryInstances, Constant, Term}
import scalus.uplc.TermDSL.given
import scalus.uplc.eval.Cek

class ContextSpec
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with ArbitraryInstances
    with scalus.ledger.api.v1.ArbitraryInstances {
    test("PubKeyHash Eq") {
        assert(PubKeyHash(ByteString.empty) === PubKeyHash(ByteString.empty))
        assert(PubKeyHash(hex"aa") !== PubKeyHash(ByteString.empty))
        assertEval(
          compile(new PubKeyHash(ByteString.empty) === new PubKeyHash(ByteString.empty)),
          true
        )
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
            import scalus.ledger.api.v1.FromDataInstances.given
            import scalus.builtin.FromDataInstances.given
            val i = fromData[Interval[POSIXTime]](d)
            i === i
        }

        forAll { (i: Interval[POSIXTime]) =>
            import scalus.ledger.api.v1.ToDataInstances.given
            import scalus.builtin.ToDataInstances.given
            assert(i === i)
            val d = i.toData
            val applied = sir $ SIR.Const(Constant.Data(d))
            assertEval(applied, true)
        }
    }

    private def assertEval(sir: SIR, expected: Term) = {
        val term = sir.toUplc()
        assert(Cek.evalUPLC(term) == expected)
    }
}
