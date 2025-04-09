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
        txInfo: TxInfo,
        sourceTxOutRef: TxOutRef
    ): Unit = {
        // val owner = datum.getOrFail("Expected datum").to[PubKeyHash]
        val Option.Some(ownerData) = datum: @unchecked
        val owner = ownerData.to[PubKeyHash]
        val signed = txInfo.signatories.contains(owner)
        require(signed, "Must be signed")
        val saysHello = redeemer.to[String] == "Hello, Cardano!"
        require(saysHello, "Invalid redeemer")
    }

}
