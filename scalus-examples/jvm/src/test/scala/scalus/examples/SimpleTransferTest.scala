package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.Data
import scalus.builtin.Data.toData
import scalus.ledger.api.v1.PubKeyHash
import scalus.ledger.api.v3.ScriptContext
import scalus.prelude.*
import scalus.testkit.ScalusTest

import scala.language.implicitConversions

class SimpleTransferTest extends AnyFunSuite with ScalusTest {

    inline given Compiler.Options = Compiler.Options(
      targetLoweringBackend = Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      // debug = false
    )

    private val sir = compile(SimpleTransfer.validate)

    private val pubKeyHash = genByteStringOfN(28).map(PubKeyHash(_)).label("PubKeyHash")
    private val owner = pubKeyHash.sample.get
    private val receiver = pubKeyHash.sample.get

    private val datum = SimpleTransfer.Datum(owner, receiver).toData
    private val deposit = SimpleTransfer.Redeemer.Deposit(1000).toData
    private val withdraw = SimpleTransfer.Redeemer.Withdraw(500).toData

    ignore("valid deposit") {
        val res = sir.runScript(context(datum, deposit, List(owner)))
        assert(res.isSuccess, res.logs)
    }

    ignore("valid withdraw") {
        val res = sir.runScript(context(datum, withdraw, List(receiver)))
        assert(res.isSuccess, res.logs)
    }

    private def context(datum: Data, redeemer: Data, signatories: List[PubKeyHash]) = {
        makeSpendingScriptContext(datum, redeemer, signatories) // todo outputs
    }

}
