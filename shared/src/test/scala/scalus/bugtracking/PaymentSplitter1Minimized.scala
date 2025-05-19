package scalus.bugtracking

import scalus.prelude.DataParameterizedValidator

import scala.math.{pow, BigInt as payees}
import scalus.*
import scalus.builtin.{Builtins, ByteString, Data}
import scalus.ledger.api.v1
import scalus.ledger.api.v1.Value.*
import scalus.ledger.api.v1.{Credential, PubKeyHash, Value}
import scalus.ledger.api.v2.TxOut
import scalus.ledger.api.v3.{TxInfo, TxOutRef}
import scalus.prelude.List.*
import scalus.prelude.Option.*
import scalus.prelude.AssocMap.*
import scalus.prelude.{*, given}

@scalus.Compile
object PaymentSplitter1Minimized extends DataParameterizedValidator {

    override def spend(
        payeesData: Data,
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {

        val payees: List[Credential.PubKeyCredential] = payeesData
            .to[List[ByteString]]
            .map(payee => Credential.PubKeyCredential(PubKeyHash(payee)))

        val myTxInputCredential =
            tx.inputs.find(_.outRef === ownRef).get.resolved.address.credential

        // Find the first and single payee that triggers the payout and pays the fee
        //  and calculate the sum of contract inputs
        val (optPayeeInputWithChange, sumContractInputs) = tx.inputs
            .foldLeft(Option.empty[TxOut], BigInt(0)) {
                case ((optTxOut, sumContractInputs), input) =>
                    if payees.contains(input.resolved.address.credential)
                    then
                        if optTxOut.isEmpty then (Some(input.resolved), sumContractInputs)
                        else fail("Already found a fee payer")
                    else if input.resolved.address.credential === myTxInputCredential then
                        (optTxOut, sumContractInputs + input.resolved.value.getLovelace)
                    else
                        // TODO: think
                        fail("Input not from the contract or payer")
            }

        val payeeInputWithChange = optPayeeInputWithChange.getOrFail(
          "One of the payees must have an input to pay the fee and trigger the payout"
        )

        val (sumOutputs, sumsPerPayee) =
            tx.outputs.foldLeft(
              (BigInt(0), AssocMap.empty[Credential.PubKeyCredential, BigInt])
            ) { case ((sum, sumsPerPayee), output) =>
                val value = output.value.getLovelace
                val payee: Credential.PubKeyCredential = output.address.credential match {
                    case Credential.PubKeyCredential(pkh) => Credential.PubKeyCredential(pkh)
                    case _                                => fail("Output to script is not allowed")
                }
                require(payees.contains(payee), "Must pay to a payee")
                val nextSum = sum + value
                AssocMap.get(sumsPerPayee)(payee) match
                    case None => (nextSum, sumsPerPayee.insert(payee, value))
                    case Some(prevSum) =>
                        (nextSum, sumsPerPayee.insert(payee, prevSum + value))
            }

        val (optSplit, optPayeeSumWithChange) =
            sumsPerPayee.toList.foldLeft(Option.empty[BigInt], Option.empty[BigInt]) {
                case ((optSplit, optPayeeSumWithChange), (payee, value)) =>
                    if payeeInputWithChange.address.credential === payee then
                        (optSplit, Some(value))
                    else
                        optSplit match
                            case None => (Some(value), optPayeeSumWithChange)
                            case Some(split) =>
                                require(split == value, "Split unequally")
                                (optSplit, optPayeeSumWithChange)
            }

        require(payees.forall(p => sumsPerPayee.get(p).isDefined), "Not all payees were paid")

        optSplit match
            case None => // one payee, no split
            case Some(split) =>
                val payeeSumWithChange = optPayeeSumWithChange.getOrFail("No change output")
                val eqSumValue = sumOutputs - payeeSumWithChange + split
                val reminder = sumContractInputs - eqSumValue
                require(reminder < payees.length, "value to be payed to payees is too low")

    }

}
