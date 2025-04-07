package scalus.examples

import scalus.*
import scalus.builtin.Data
import scalus.builtin.FromDataInstances.given
import scalus.ledger.api.v3.{PubKeyHash, ScriptContext, ScriptInfo, TxInfo, TxOutRef}
import scalus.ledger.api.v1.FromDataInstances.given
import scalus.ledger.api.v3.FromDataInstances.given
import scalus.ledger.api.v3.ScriptInfo.SpendingScript
import scalus.ledger.api.v3.{PubKeyHash, ScriptContext, ScriptInfo}
import scalus.prelude.*
import scalus.prelude.Prelude.*

@Compile
object HelloCardano extends Validator {

    override def spend(
                          datum: Option[Data],
                          redeemer: Data,
                          targetTxInfo: TxInfo,
                          sourceTxOutRef: TxOutRef
                      ): Boolean = {
        datum match
            case Option.Some(ownerDatum) =>
                val owner = ownerDatum.to[PubKeyHash]
                // must be signed
                val unused = targetTxInfo.signatories
                    .find(signatory => signatory.hash == owner.hash)
                    .getOrFail("No signature found")
                val mustSayHello = redeemer.to[String] == "Hello, Cardano!"
                if !mustSayHello then throw new Exception("Invalid message")
                true
            case Option.None =>
                throw new Exception("Expected datum")
    }

}

    /*
    def validator(scriptContext: Data): Unit = {
        val ctx = scriptContext.to[ScriptContext]
        ctx.scriptInfo match
            case SpendingScript(_, datum) =>
                val owner = datum.getOrFail("Expected datum").to[PubKeyHash]
                val signed = ctx.txInfo.signatories.contains(owner)
                require(signed, "Must be signed")
                val saysHello = ctx.redeemer.to[String] == "Hello, Cardano!"
                require(saysHello, "Invalid redeemer")
            case _ => fail("Must be spending")
    }

     */
//}
