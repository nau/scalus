package scalus.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.*
import scalus.builtin.Data.{fromData, toData}
import scalus.sir.*
import scalus.ledger.api.v3.*
import scalus.uplc.Term
import scalus.uplc.eval.PlutusVM
import scalus.uplc.eval.Result

class SirToUplcFromDataReplacementTest extends AnyFunSuite {

    given PlutusVM = PlutusVM.makePlutusV3VM()

    test("Import ScriptContext from Data") {
        val sir = compile { (scData: Data) =>
            val sc = Data.fromData[ScriptContext](scData)

        }
        println(sir.showHighlighted)
        val lowering = SirToUplcV3Lowering(sir)
        val term = lowering.lower()
        println(term.showHighlighted)
        println(term.evaluateDebug)
    }

    test("run Hello World") {
        import scalus.prelude.*
        val sir = compile { (scData: Data) =>
            val sc = Data.fromData[ScriptContext](scData)
            val tx = sc.txInfo
            sc.scriptInfo match {
                case ScriptInfo.SpendingScript(txOutRef, datum) =>
                    val Option.Some(ownerData) = datum: @unchecked
                    val owner = ownerData.to[PubKeyHash]
                    val signed = tx.signatories.contains(owner)
                    require(signed, "Must be signed")
                    val saysHello = sc.redeemer.to[String] == "Hello, World!"
                    require(saysHello, "Invalid redeemer")
            }
        }
        println(sir.showHighlighted)
        val lowering = SirToUplcV3Lowering(sir)
        val term = lowering.lower()
        println(term.showHighlighted)
        println(term.evaluateDebug)
    }

}
