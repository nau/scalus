package scalus.examples

import scalus.*
import scalus.builtin.Data
import scalus.builtin.FromDataInstances.given
import scalus.ledger.api.v1.FromDataInstances.given
import scalus.ledger.api.v3.FromDataInstances.given
import scalus.ledger.api.v3.ScriptInfo.SpendingScript
import scalus.ledger.api.v3.{PubKeyHash, ScriptContext, ScriptInfo}
import scalus.prelude.*
import scalus.prelude.Prelude.*

@Compile
object HelloCardano {
    def validator(scriptContext: Data): Unit = {
        val ctx = scriptContext.to[ScriptContext]
        ctx.scriptInfo match
            case SpendingScript(_, datum) =>
                val owner = datum.getOrFail("Expected datum").to[PubKeyHash]
                val mustBeSigned = ctx.txInfo.signatories.find { _.hash == owner.hash }
                mustBeSigned orFail "Must be signed"
                val mustSayHello = ctx.redeemer.to[String] == "Hello, Cardano!"
                require(mustSayHello, "Invalid redeemer")
            case _ => fail("Must be spending")
    }
}
