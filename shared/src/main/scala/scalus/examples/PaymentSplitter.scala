package scalus.examples

import scalus.*
import scalus.builtin.{ByteString, Data}
import scalus.builtin.FromDataInstances.given
import scalus.ledger.api.v1
import scalus.ledger.api.v1.Value.{*, given}
import scalus.ledger.api.v1.{Credential, PubKeyHash, Value}
import scalus.ledger.api.v3.FromDataInstances.given
import scalus.ledger.api.v3.{ScriptContext, ScriptInfo, TxInfo, TxOutRef}
import scalus.ledger.api.v3.TxOutRef.given
import scalus.prelude.List.*
import scalus.prelude.Prelude.*
import scalus.prelude.{List, *}

@Compile
object PaymentSplitter {
    type PayeeHash = ByteString
    def validator(payeesData: Data)(scriptContext: Data): Unit = {
        val ctx: ScriptContext = scriptContext.to[ScriptContext]
        ctx.scriptInfo match
            case ScriptInfo.SpendingScript(txOutRef, _) =>
                val payees = payeesData.toList.head
                    .to[List[ByteString]]
                    .map(payee => Credential.PubKeyCredential(PubKeyHash(payee)))
                spend(txOutRef, ctx.txInfo, payees)
            case _ => fail("Must be spending")
    }

    def spend(myTxOutRef: TxOutRef, txInfo: TxInfo, payees: List[Credential]): Unit = {
        // aggregate outputs and their values
        val outputValues =
            val groupedOutputs = txInfo.outputs.groupBy(_.address.credential)
            AssocMap.map(groupedOutputs) { (credential, outputs) =>
                val sum = outputs.foldLeft(Value.zero)((acc, txout) => acc + txout.value)
                (credential, sum)
            }
        val ownInputCredential = txInfo.inputs
            .find(_.outRef === myTxOutRef)
            .getOrFail("Impossible: couldn't find own input")
            .resolved
            .address
            .credential
        val inputWithChange = txInfo.inputs match
            case List.Cons(firstInput, tail) =>
                tail match
                    case List.Cons(secondInput, tail) =>
                        tail match
                            case List.Nil =>
                                if firstInput.resolved.address.credential === ownInputCredential
                                then
                                    if payees.contains(secondInput.resolved.address.credential) then
                                        secondInput
                                    else fail("Only payees can trigger payout")
                                else if secondInput.resolved.address.credential === ownInputCredential
                                then
                                    if payees.contains(firstInput.resolved.address.credential) then
                                        firstInput
                                    else fail("Only payees can trigger payout")
                                else fail("Impossible: one of the inputs must be the own input")
                            case _ => fail("Must be 2 inputs")
                    case _ => fail("Must be 2 inputs")
            case _ => fail("Inputs can't be empty")

        val (firstPayerCredential, firstPayerValue) = outputValues.inner.head
        val splitValue =
            if firstPayerCredential !== inputWithChange.resolved.address.credential then
                firstPayerValue
            else firstPayerValue - inputWithChange.resolved.value + Value.lovelace(txInfo.fee)

        outputValues.inner.foldLeft(payees) { case (payees, (cred, value)) =>
            require(value === splitValue, "Split unequally")
            // Here we require that all payees are being paid
            // We expect the same order of outputs as listed in payees
            payees match
                case List.Nil => fail("More outputs than payees")
                case List.Cons(payee, tail) =>
                    require(cred === payee, "Must pay to a payee")
                    tail
        }
        /*
        NOTE: This code allows non-unique payess, messing up payments

        let has_no_additional_payees =
          list.difference(list.unique(output_credentials), payees) == []


         * Alice: 10
              Bob: 10
              Charlie: 10
            =====
            Contract UTXO: 30

            Unlock:

            Contract (30) -->   --> Alice 17 (17 - 10 + 2 == 9)
            Alice (10)    -->   --> Bob 9
            Fee: 2              --> Charlie 9

            Contract (30) -->   --> Alice   9
                          -->   --> Bob     9
            Fee: 3              --> Charlie 9

            Contract (30) -->   --> Alice   9
                          -->   --> Bob     9
            Fee: 2              --> Charlie 9
            Always fails, because

         *
         *
         * */

    }
}
