package scalus.examples

import scalus.*
import scalus.builtin.FromDataInstances.given
import scalus.builtin.{ByteString, Data}
import scalus.ledger.api.v1
import scalus.ledger.api.v1.{Credential, PubKeyHash, Value}
import scalus.ledger.api.v1.Value.*
import scalus.ledger.api.v2.TxOut
import scalus.ledger.api.v3.FromDataInstances.given
import scalus.ledger.api.v3.{ScriptContext, ScriptInfo, TxInInfo, TxInfo, TxOutRef}
import scalus.prelude.*
import scalus.prelude.List.*
import scalus.prelude.Option.*
import scalus.prelude.Prelude.*

/** Split payouts equally among a list of specified payees
  *
  * A payment splitter can be used for example to create a shared project donation address, ensuring
  * that all payees receive the same amount
  *
  * Sending lovelace to the contract works similarly to sending lovelace to any other address. The
  * payout transaction can only be submitted by one of the payees, and the output addresses are
  * restricted to the payees. The output sum must be equally divided to ensure the transaction is
  * successful.
  *
  * @see
  *   [[https://meshjs.dev/smart-contracts/payment-splitter]]
  */
@Compile
object PaymentSplitter extends ParametrizedValidator[List[Credential]] {

    /** @param payeesData
      *   List of payees list to split the payment to.
      * @param scriptContext
      *   [[ScriptContext]]
      *
      * @example
      *   {{{
      *     val payees = List(A, B, C)
      *     val script = PaymentSplitter.validator(payees)
      *   }}}
      */
    override def spend(
        payees: List[Credential],
        datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        sourceTxOutRef: TxOutRef
    ): Unit = {

        // Note, that this expression is for compability with the data parametrization as in aiken.
        //  Without compabilty requirement,  we can accept List[Credential] directly without this transformation
        //   using ParametrizedValidator[List[Credential]].
        // val payees = payeesData.toList.head
        //    .to[List[ByteString]]
        //    .map(payee => Credential.PubKeyCredential(PubKeyHash(payee)))

        // Find the first and single payee that triggers the payout and pays the fee
        val payeeInputWithChange = tx.inputs
            .foldLeft(Option.empty[TxOut]) { (txOut, input) =>
                if payees.contains(input.resolved.address.credential)
                then
                    if txOut.isEmpty then Some(input.resolved)
                    else fail("Already found a fee payer")
                else txOut
            }
            .getOrFail("One of the payees must have an input to pay the fee and trigger the payout")

        val (unpaidPayees, _) = tx.outputs.foldLeft((payees, Option.empty[BigInt])) {
            case ((payees, prevValue), output) =>
                val split =
                    if payeeInputWithChange.address.credential === output.address.credential
                    then
                        val change = payeeInputWithChange.value.getLovelace - tx.fee
                        output.value.getLovelace - change
                    else output.value.getLovelace

                require(prevValue.forall(_ == split), "Split unequally")

                // Here we check that all outputs pay to payees from the list
                // We expect the same order of outputs as listed in payees
                payees match
                    case Nil => fail("More outputs than payees")
                    case Cons(payee, tail) =>
                        require(output.address.credential === payee, "Must pay to a payee")
                        (tail, Some(split))
        }
        require(unpaidPayees.isEmpty, "Not all payees were paid")
    }
}
