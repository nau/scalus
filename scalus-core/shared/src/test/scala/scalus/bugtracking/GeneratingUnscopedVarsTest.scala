package scalus.bugtracking

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

import org.scalatest.funsuite.AnyFunSuite

@Compile
object Min20250702 {

    def validate(param: Data): Unit = {
        // val (q1, q2) = List
        //    .empty[BigInt]
        //    .foldLeft(BigInt(0), BigInt(0)) { case (a, value) =>
        //        (BigInt(0), value)
        //    }
        val (q1, q2) = List
            .empty[BigInt]
            .foldLeft(BigInt(0), BigInt(0)) { case (a, value) =>
                (BigInt(0), value)
            }
    }

}

class GeneratingUnscopedVarsTest extends AnyFunSuite {

    inline given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    test("Min20250702 should compile and transform to UPLC") {
        // pending
        val sir = compile {
            Min20250702.validate
        }
        println(sir.pretty.render(100))
        val uplc = sir.toUplc(true)
        // val result = script.evaluateDebug
        // assert(result.isSuccess)
    }

}
