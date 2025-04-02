package scalus.examples

import scalus.*
import scalus.Compiler.compile
import scalus.builtin.Data
import scalus.ledger.api.v3.{PubKeyHash, ScriptContext, ScriptInfo}
import scalus.ledger.api.v1.FromDataInstances.given
import scalus.ledger.api.v3.FromDataInstances.given
import scalus.builtin.FromDataInstances.given
import scalus.prelude.*

@Compile
object HelloCardano {
    def validator(scriptContext: Data): Unit = {
        val ctx = scriptContext.to[ScriptContext]
        ctx.scriptInfo match
            case ScriptInfo.SpendingScript(txOutRef, datum) =>
                datum match
                    case Option.Some(ownerDatum) =>
                        val owner = ownerDatum.to[PubKeyHash]
                        // must be signed
                        ctx.txInfo.signatories.find { _.hash == owner.hash }.getOrFail()
                        val mustSayHello = ctx.redeemer.to[String] == "Hello, Cardano!"
                        if !mustSayHello then throw new Exception("Invalid message")
                    case Option.None => throw new Exception("Expected datum")
            case _ => throw new Exception("Invalid script type")
    }
}
