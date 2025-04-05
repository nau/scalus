package scalus.examples

import scalus.*
import scalus.builtin.{ByteString, Data}
import scalus.builtin.FromDataInstances.given
import scalus.ledger.api.v1
import scalus.ledger.api.v1.{Credential, PubKeyHash, Value}
import scalus.ledger.api.v2.TxOut
import scalus.ledger.api.v3.FromDataInstances.given
import scalus.ledger.api.v3.{ScriptContext, ScriptInfo, TxInInfo, TxInfo, TxOutRef}
import scalus.ledger.api.v3.TxOutRef.given
import scalus.prelude.*
import scalus.prelude.List.*
import scalus.prelude.Option.*
import scalus.prelude.Prelude.*

import scala.annotation.tailrec

//noinspection NoTailRecursionAnnotation
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
        val payeeInputWithChange =
            def findPayeeOrFail(inputs: List[TxInInfo], feePayer: Option[TxOut]): TxOut =
                inputs match
                    case Nil =>
                        feePayer.getOrFail(
                          "One of the payees must have an input to pay the fee and trigger the payout"
                        )
                    case Cons(input, tail) =>
                        if payees.contains(input.resolved.address.credential) then
                            feePayer match
                                case None => findPayeeOrFail(tail, Option.Some(input.resolved))
                                case _    => fail("Already found a fee payer")
                        else findPayeeOrFail(tail, feePayer)
            findPayeeOrFail(txInfo.inputs, None)

        val (unpaidPayees, _) = txInfo.outputs.foldLeft((payees, None: Option[BigInt])) {
            case ((payees, prevValue), output) =>
                val splitted =
                    if payeeInputWithChange.address.credential === output.address.credential
                    then
                        val change = payeeInputWithChange.value.lovelace - txInfo.fee
                        output.value.lovelace - change
                    else output.value.lovelace

                prevValue match
                    case None       =>
                    case Some(prev) => require(splitted == prev, "Split unequally")

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

    def spend2(ownScriptRef: TxOutRef, txInfo: TxInfo, payees: List[Credential]): Unit = {
        // We require a transaction with only 2 inputs: locked script input and one of the payees
        // The payee pays the fee and receives the change
        val inputWithChange = txInfo.inputs match
            case Cons(firstInput, tail) =>
                tail match
                    case Cons(secondInput, tail) =>
                        tail match
                            case Nil =>
                                if ownScriptRef === firstInput.outRef then secondInput.resolved
                                else if ownScriptRef === secondInput.outRef then firstInput.resolved
                                else fail("Impossible: one of the inputs must be the own input")
                            case _ => fail("Must be 2 inputs")
                    case _ => fail("Must be 2 inputs")
            case _ => fail("Inputs can't be empty")

        require(
          payees.contains(inputWithChange.address.credential),
          "Only payees can trigger payout"
        )

        val firstOutput = txInfo.outputs.head
        val splitValue =
            if firstOutput.address.credential !== inputWithChange.address.credential then
                firstOutput.value.lovelace
            else firstOutput.value.lovelace - inputWithChange.value.lovelace + txInfo.fee

        val unpaidPayees = txInfo.outputs.foldLeft(payees) { case (payees, output) =>
            require(output.value.lovelace == splitValue, "Split unequally")
            // Here we require that all payees are being paid
            // We expect the same order of outputs as listed in payees
            payees match
                case Nil => fail("More outputs than payees")
                case Cons(payee, tail) =>
                    require(output.address.credential === payee, "Must pay to a payee")
                    tail
        }
        require(unpaidPayees.isEmpty, "Not all payees were paid")

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

            Contract (30) -->   --> Alice 17 (17 - (10 - 2) == 9)
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
