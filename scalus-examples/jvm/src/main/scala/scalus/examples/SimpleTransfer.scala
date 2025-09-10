package scalus.examples

import scalus.Compile
import scalus.builtin.{Data, FromData, ToData}
import scalus.ledger.api.v1.{Credential, PubKeyHash}
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.prelude.*

/** https://github.com/blockchain-unica/rosetta-smart-contracts/tree/main/contracts/simple_transfer
  */
@Compile
object SimpleTransfer extends Validator {

    case class Config(
        owner: PubKeyHash,
        recipient: PubKeyHash
    ) derives ToData,
          FromData

    enum Action derives ToData, FromData {
        case Deposit(amount: Lovelace)
        case Withdraw(amount: Lovelace)
    }

    private def lookupTx(tx: TxInfo, cred: Credential): (List[TxInInfo], List[TxOut]) = (
      tx.inputs.filter(_.resolved.address.credential === cred),
      tx.outputs.filter(_.address.credential === cred)
    )

    private def outputsAda(outputs: List[TxOut]): Lovelace = {
        outputs.map(_.value.getLovelace).foldLeft(BigInt(0))(_ + _)
    }

    private def inputsAda(inputs: List[TxInInfo]): Lovelace = {
        inputs.map(_.resolved.value.getLovelace).foldLeft(BigInt(0))(_ + _)
    }

    override def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val contract = tx.findOwnInput(ownRef).get.resolved
        val balance = contract.value.getLovelace
        val (contractInputs, contractOutputs) = lookupTx(tx, contract.address.credential)
        val Config(owner, recipient) = datum.get.to[Config]

        val action = redeemer.to[Action]
        action match
            case Action.Deposit(amount) =>
                require(tx.signatories.contains(owner), "Deposit must be signed by owner")
                // eliminate double satisfaction by ensuring exactly one contract input and one output
                require(contractInputs.size == BigInt(1), "Contract output missing")
                require(contractOutputs.size == BigInt(1), "Contract output missing")
                require(
                  outputsAda(contractOutputs) === balance + amount,
                  "Contract has received incorrect amount"
                )
                val expectedDatum = OutputDatum.OutputDatum(datum.get)
                val contractOutput = contractOutputs.head
                require(contractOutput.datum === expectedDatum, "Output datum changed")
            case Action.Withdraw(withdraw) =>
                require(tx.signatories.contains(recipient), "Withdraw must be signed by recipient")
                require(contractInputs.size == BigInt(1), "Contract output missing")
                if withdraw < balance then
                    // eliminate double satisfaction by ensuring exactly one contract input and one output
                    require(contractOutputs.size == BigInt(1), "Contract output missing")
                    require(
                      outputsAda(contractOutputs) === balance - withdraw,
                      "Contract balance is incorrect"
                    )
                    val expectedDatum = OutputDatum.OutputDatum(datum.get)
                    val contractOutput = contractOutputs.head
                    require(contractOutput.datum === expectedDatum, "Output datum changed")
                else if withdraw == balance then
                    // if withdrawing all, there should be no contract output
                    require(contractOutputs.isEmpty, "Contract output not empty")
                else fail("Withdraw exceeds balance")
    }
}
