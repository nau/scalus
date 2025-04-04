package scalus.examples

import scalus.*
import scalus.builtin.{ByteString, Data}
import scalus.builtin.FromDataInstances.given
import scalus.ledger.api.v1.Value.{*, given}
import scalus.ledger.api.v1.{Credential, Value}
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
                val payees = payeesData.toList.head.to[List[ByteString]]
                spend(txOutRef, ctx.txInfo, payees)
            case _ => fail("Must be spending")
    }

    def spend(myTxOutRef: TxOutRef, txInfo: TxInfo, payees: List[PayeeHash]): Unit = {
        // Only the payees are allowed to trigger the payout and the list needs to include all payees
        val outputCredentials = txInfo.outputs.map(_.address.credential)

        val groupedOutputs = txInfo.outputs.groupBy(_.address.credential)
        val groupedInputs = txInfo.inputs.groupBy(_.resolved.address.credential)
        val outputValues = AssocMap.map(groupedOutputs) { (credential, outputs) =>
            val sum = outputs.foldLeft(Value.zero)((acc, txout) => acc + txout.value)
            (credential, sum)
        }
        val inputValues = AssocMap.map(groupedInputs) { (credential, outputs) =>
            val sum = outputs.foldLeft(Value.zero)((acc, txout) => acc + txout.resolved.value)
            (credential, sum)
        }
        val ownInput = txInfo.inputs.find(_.outRef === myTxOutRef).getOrFail("AAA")
        val (inputWithChangeCredential, inputWithChangeValue) = inputValues.inner match
            case List.Cons(a, tail) =>
                tail match
                    case List.Cons(b, tail) =>
                        tail match
                            case List.Nil =>
                                if a._1 === ownInput.resolved.address.credential then
                                    if payees.contains(b._1) then b
                                    else fail("Only payees can trigger payout")
                                else if b._1 === ownInput.resolved.address.credential then
                                    if payees.contains(a._1) then a
                                    else fail("Only payees can trigger payout")
                                else fail("DDD")
                            case _ => fail("Must be 2 inputs")
                    case _ => fail("Must be 2 inputs")
            case _ => fail("Inputs can't be empty")

        val (firstPayerCredential, firstPayerValue) = outputValues.inner.head
        val splitValue =
            if firstPayerCredential !== inputWithChangeCredential then firstPayerValue
            else firstPayerValue - inputWithChangeValue + Value.lovelace(txInfo.fee)

        val splitEqualy = outputValues.inner.forall { (_, value) =>
            value === splitValue
        }
        splitEqualy orFail "NOPE"
//        require(outputCredentials)

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
