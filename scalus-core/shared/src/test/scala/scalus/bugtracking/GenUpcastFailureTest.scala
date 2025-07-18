package scalus.bugtracking

import org.scalatest.funsuite.AnyFunSuite

import scalus.Compiler.compile
import scalus.*
import scalus.prelude.{*, given}
import scalus.builtin.{ByteString, Data}
import scalus.ledger.api.v1.{Credential, PubKeyHash}
import scalus.ledger.api.v1.Value.getLovelace
import scalus.ledger.api.v2.TxOut
import scalus.ledger.api.v3.*
import scalus.prelude.{AssocMap, DataParameterizedValidator, List, Option}
import scalus.prelude.Option.Some

class GenUpcastFailureTest extends AnyFunSuite {

    inline given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    test("Call of getLovelace should compile and transform to UPLC") {
        // pending
        val sir = compile { (value: Value) =>
            val lv = value.getLovelace
            if lv < 10 then scalus.prelude.fail("lv < 10")
        }
        // println(sir.pretty.render(100))
        val uplc = sir.toUplc(generateErrorTraces = true)
        // println(uplc.pretty.render(100))
        // val result = script.evaluateDebug
        // assert(result.isSuccess)
    }

}
