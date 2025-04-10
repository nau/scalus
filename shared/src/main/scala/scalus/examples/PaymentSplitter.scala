package scalus.examples

import scalus.*
import scalus.builtin.FromDataInstances.given
import scalus.builtin.{ByteString, Data}
import scalus.ledger.api.v1
import scalus.ledger.api.v1.{Credential, PubKeyHash, Value}
import scalus.ledger.api.v2.TxOut
import scalus.ledger.api.v3.FromDataInstances.given
import scalus.ledger.api.v3.{ScriptContext, ScriptInfo, TxInInfo, TxInfo}
import scalus.prelude.*
import scalus.prelude.List.*
import scalus.prelude.Option.*
import scalus.prelude.Prelude.*

@Compile
object PaymentSplitter {
    def validator(payeesData: Data)(scriptContext: Data): Unit = {
        val ctx: ScriptContext = scriptContext.to[ScriptContext]
        ctx.scriptInfo match
            case ScriptInfo.SpendingScript(_, _) =>
                val payees = payeesData.toList.head
                    .to[List[ByteString]]
                    .map(payee => Credential.PubKeyCredential(PubKeyHash(payee)))
                spend(ctx.txInfo, payees)
            case _ => fail("Must be spending")
    }

    extension (value: Value)
        def lovelace: BigInt = value.toList match
            case Nil                   => 0
            case Cons((cs, tokens), _) =>
                // Ada is always the first token. Only Ada can have empty CurrencySymbol. And its only token is Lovelace
                if cs == ByteString.empty then tokens.toList.head._2
                else 0

    def spend(txInfo: TxInfo, payees: List[Credential]): Unit = {
        val payeeInputWithChange = txInfo.inputs
            .foldLeft(Option.empty[TxOut]) { (txOut, input) =>
                if payees.contains(input.resolved.address.credential)
                then
                    if txOut.isEmpty then Some(input.resolved)
                    else fail("Already found a fee payer")
                else txOut
            }
            .getOrFail("One of the payees must have an input to pay the fee and trigger the payout")

        val (unpaidPayees, _) = txInfo.outputs.foldLeft((payees, Option.empty[BigInt])) {
            case ((payees, prevValue), output) =>
                val splitted =
                    if payeeInputWithChange.address.credential === output.address.credential
                    then
                        val change = payeeInputWithChange.value.lovelace - txInfo.fee
                        output.value.lovelace - change
                    else output.value.lovelace

                prevValue.requireForall(_ == splitted, "Split unequally")

                // Here we check that all outputs pay to payees from the list
                // We expect the same order of outputs as listed in payees
                payees match
                    case Nil => fail("More outputs than payees")
                    case Cons(payee, tail) =>
                        require(output.address.credential === payee, "Must pay to a payee")
                        (tail, Some(splitted))
        }
        require(unpaidPayees.isEmpty, "Not all payees were paid")
    }
}
