package scalus.examples

import scalus.*
import scalus.Compiler.compile
import scalus.builtin.ByteString.*
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.ledger.api.v1.PubKeyHash
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.uplc.*
import scalus.uplc.eval.*

import scala.language.implicitConversions
import scala.math.Ordering.Implicits.*

class HelloCardanoSpec extends ExampleTest {

    test("Hello Cardano") {
        val ownerPubKey = PubKeyHash(hex"1234567890abcdef1234567890abcdef1234567890abcdef12345678")
        val message = "Hello, World!".toData
        val context = makeSpendingScriptContext(
          datum = ownerPubKey.toData,
          redeemer = message,
          signatories = List(ownerPubKey)
        )

        val result = compile(HelloCardano.validate).runScript(context)

        assert(result.isSuccess)
        assert(result.budget == ExBudget(ExCPU(63687411L), ExMemory(238149L)))
    }

    // https://aiken-lang.org/example--hello-world/basics
    test("hello_world_example") {
        val ownerPubKey = PubKeyHash(hex"00000000000000000000000000000000000000000000000000000000")
        val message = "Hello, World!".toData
        val context = ScriptContext(
            txInfo = TxInfo.placeholder.copy(signatories = List(ownerPubKey)),
            redeemer = message,
            scriptInfo = ScriptInfo.SpendingScript(
                txOutRef = TxOutRef(id = TxId(hex""), idx = 0),
                datum = Option.Some(ownerPubKey.toData)
            )
        )

        val result = compile(HelloCardano.validate).runScript(context)
        result.check(
            testName = "HelloCardanoSpec.hello_world_example",
            scalusBudget = ExBudget(ExCPU(53180220L), ExMemory(198865L)),
            aikenBudget = ExBudget(ExCPU(9375627L), ExMemory(28554L)),
            isPrintComparison = false
        )
    }
}
