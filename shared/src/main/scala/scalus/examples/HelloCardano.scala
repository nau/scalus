package scalus.examples

import scalus.*
import scalus.builtin.Data
import scalus.builtin.FromDataInstances.given
import scalus.ledger.api.v1.FromDataInstances.given
import scalus.ledger.api.v3.FromDataInstances.given
import scalus.ledger.api.v3.{PubKeyHash, ScriptContext, ScriptInfo}
import scalus.prelude.*
import scalus.prelude.Prelude.*

@Compile
object HelloCardano {
    def validator(scriptContext: Data): Unit = {
        val ctx = scriptContext.to[ScriptContext]
        ctx.scriptInfo match
            case ScriptInfo.SpendingScript(txOutRef, datum) =>
                datum match
                    case Option.Some(ownerDatum) =>
                        val owner = ownerDatum.to[PubKeyHash]
                        // must be mustBeSigned
                        val mustBeSigned = ctx.txInfo.signatories.find { _.hash == owner.hash }
                        mustBeSigned orFail "Must be mustBeSigned"
                        val mustSayHello = ctx.redeemer.to[String] == "Hello, Cardano!"
                        require(mustSayHello, "Fail")
                        mustSayHello orFail "Fail"
                    case Option.None => throw new Exception("Expected datum")
            case _ => throw new Exception("Invalid script type")
    }
}
