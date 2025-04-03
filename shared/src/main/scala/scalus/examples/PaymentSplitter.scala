package scalus.examples

import scalus.*
import scalus.Compiler.compile
import scalus.builtin.Data
import scalus.builtin.ByteString
import scalus.builtin.FromDataInstances.given
import scalus.ledger.api.v1.FromDataInstances.given
import scalus.ledger.api.v3.FromDataInstances.given
import scalus.ledger.api.v3.{PubKeyHash, ScriptContext, ScriptInfo, TxInfo}
import scalus.prelude.*

case class Redeemer(message: ByteString)

@Compile
object PaymentSplitter {
    type PayeeHash = ByteString
    def validator(payeesData: Data)(scriptContext: Data): Unit = {
        val ctx: ScriptContext = scriptContext.to[ScriptContext]
        ctx.scriptInfo match
            case ScriptInfo.SpendingScript(_, _) =>
                val payees = payeesData.toList.head.to[List[ByteString]]
                spend(ctx.txInfo, payees)
            case _ => error("Must be spending")
    }

    def spend(txInfo: TxInfo, payees: List[PayeeHash]): Unit = {
        // Only the payees are allowed to trigger the payout and the list needs to include all payees
//        val has_no_additional_payees =
//            list.difference(list.unique(output_credentials), payees) == []
        val outputCredentials = txInfo.outputs.map(_.address.credential)
//            list.map(outputs, fn(output) { output.address.payment_credential })
//        require(
    }
}
