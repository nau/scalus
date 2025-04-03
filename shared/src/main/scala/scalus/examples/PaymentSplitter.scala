package scalus.examples

import scalus.*
import scalus.Compiler.compile
import scalus.builtin.Data
import scalus.builtin.ByteString
import scalus.builtin.FromDataInstances.given
import scalus.ledger.api.v1.FromDataInstances.given
import scalus.ledger.api.v1.{Credential, Value}
import Value.{*, given}
import scalus.ledger.api.v3.FromDataInstances.given
import scalus.ledger.api.v3.{PubKeyHash, ScriptContext, ScriptInfo, TxInfo, TxOutRef}
import scalus.ledger.api.v3.TxOutRef.given
import scalus.prelude.{List, *}
import scalus.prelude.Prelude.*
import List.*

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
        val outputValues = AssocMap.map(groupedOutputs): (credential, outputs) =>
            val sum = outputs.foldLeft(Value.zero)((acc, txout) => acc + txout.value)
            (credential, sum)
        val inputValues = AssocMap.map(groupedInputs): (credential, outputs) =>
            val sum = outputs.foldLeft(Value.zero)((acc, txout) => acc + txout.resolved.value)
            (credential, sum)
        val myInput = txInfo.inputs.find(_.outRef === myTxOutRef).getOrFail("AAA")

        val inputWithFee: Option[(Credential, Value)] = inputValues.inner match
            case List.Cons(a, tail) =>
                tail match
                    case List.Nil =>
                        // the only input is our script
                        a._1 === myInput.resolved.address.credential orFail "CCC"
                        Option.None
                    case List.Cons(b, tail) =>
                        tail match
                            case List.Nil =>
                                if a._1 === myInput.resolved.address.credential then
                                    if payees.contains(b._1) then Option.Some(b) else fail("nahuy")
                                else if b._1 === myInput.resolved.address.credential then
                                    if payees.contains(a._1) then Option.Some(a) else fail("nahuy")
                                else fail("DDD")
                            case _ => fail("BBB")
                    case _ => fail("BBB")
            case _ => fail("BBB")

        val firstPayer = outputValues.inner.head
        val splitValue = inputWithFee match
            case Option.None => firstPayer._2
            case Option.Some(inputWithFee) =>
                if firstPayer._1 !== inputWithFee._1 then firstPayer._2
                else firstPayer._2 - inputWithFee._2 + Value.lovelace(txInfo.fee)

        val splitEqualy = outputValues.inner.forall: (cred, value) =>
            value === splitValue
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
