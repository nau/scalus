package scalus.examples

import scalus.*
import scalus.builtin.FromDataInstances.given
import scalus.builtin.{ByteString, Data}
import scalus.builtin.Builtins
import scalus.ledger.api.v1
import scalus.ledger.api.v1.{Credential, PubKeyHash, Value}
import scalus.ledger.api.v1.Value.*
import scalus.ledger.api.v2.TxOut
import scalus.ledger.api.v3.{TxInfo, TxOutRef}
import scalus.prelude.*
import scalus.prelude.List.*
import scalus.prelude.Option.*
import scalus.prelude.Prelude.{*, given}

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
object PaymentSplitterDI extends DataParametrizedValidator {

    /** @param payeesData
      *   List of payees list to split the payment to.
      * @param scriptContext
      *   [[ScriptContext]]
      * @example
      *   {{{
      *     val payees = List(A, B, C)
      *     val script = PaymentSplitter.validator(payees.toData)
      *   }}}
      */
    override def spend(
        payeesData: Data,
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        sourceTxOutRef: TxOutRef
    ): Unit = {

        // Note, that this expression is for compability with the data parametrization as in aiken.
        //  Without compabilty requirement,  we can accept List[Credential] directly without this transformation
        //   using ParametrizedValidator[List[Credential]].
        val payees = payeesData.toList.head
            .to[List[ByteString]]
            .map(payee => Credential.PubKeyCredential(PubKeyHash(payee)))

        val myTxInputCredential = tx.inputs
            .find(_.outRef === sourceTxOutRef)
            .getOrFail("No output to the contract")
            .resolved
            .address
            .credential

        // Find the first and single payee that triggers the payout and pays the fee
        //  and calculate the sum of contract inputs
        val (optPayeeInputWithChange, sumContractInputs) = tx.inputs
            .foldLeft(Option.empty[TxOut], BigInt(0)) {
                case ((optTxOut, sumContractInputs), input) =>
                    if payees.contains(input.resolved.address.credential)
                    then
                        if optTxOut.isEmpty then (Some(input.resolved), sumContractInputs)
                        else fail("Already found a fee payer")
                    else if (input.resolved.address.credential === myTxInputCredential)
                        (optTxOut, sumContractInputs + input.resolved.value.getLovelace)
                    else
                        // TODO: think
                        fail("Input not from the contract or payer")
            }

        val payeeInputWithChange = optPayeeInputWithChange.getOrFail(
          "One of the payees must have an input to pay the fee and trigger the payout"
        )

        val (unpaidPayees, optSplit, optPayeeOutputWithChange, sumOutput, nOutputs) =
            tx.outputs.foldLeft(
              (payees, Option.empty[BigInt], Option.empty[TxOut], BigInt(0), BigInt(0))
            ) { case ((payees, optPrevSplit, optPayWithChange, sum, nOutputs), output) =>
                val value = output.value.getLovelace
                payees match
                    case Nil => fail("More outputs than payees")
                    case Cons(payee, payeesTail) =>
                        require(
                          output.address.credential === payee,
                          "Must pay to a payee"
                        )
                        val nextSum = sum + value
                        if payeeInputWithChange.address.credential === output.address.credential
                        then (payeesTail, optPrevSplit, Some(output), nextSum, nOutputs + 1)
                        else
                            require(optPrevSplit.forall(value === _), "Split unequally")
                            (payeesTail, Some(value), optPayWithChange, nextSum, nOutputs + 1)
            }

        require(unpaidPayees.isEmpty, "Not all payees were paid")
        optSplit match
            case None => // one payee, no split
            case Some(split) =>
                val payeeOutputWithChange = optPayeeOutputWithChange.getOrFail("No change output")
                val eqSumValue = sumOutput - payeeOutputWithChange.value.getLovelace + split
                val reminder = sumContractInputs - eqSumValue
                require(
                  reminder < nOutputs,
                  "value to be payed to payees is too low"
                  // s"""reminder (${reminder}) must be less than nOutputs (${nOutputs},
                  //  sumOutput (${sumOutput}), eqSumValue (${eqSumValue}), split (${split}))
                  //  """
                )
            //    nOutputs * (split + 1) > sumContractInputs   <=>
            //    nOutputs * split + nOutputs > sumContractInputs <=>
            //    eqSumValue + nOutputs > sumContractInputs <=>
            //    nOutputs > reminder ( = sumContractInputs - eqSumValue)
            //
        // max nPayers  approx 570
        //   570 lovelace less then fee.  So, we can not think
        //   570 lovelace less then fee.  So, we can not think
        //
    }

}
