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
        case Deposit(amount: Value)
        case Withdraw(amount: Value)
    }

    private def lookupTx(tx: TxInfo, cred: Credential): (List[TxInInfo], List[TxOut]) = (
      tx.inputs.filter(_.resolved.address.credential === cred),
      tx.outputs.filter(_.address.credential === cred)
    )

    private def countValue[T](a: List[T])(f: T => Value): Value =
        a.map(f).foldLeft(Value.zero)(_ + _)

    private def outputsValue(outputs: List[TxOut]): Value =
        countValue(outputs)(_.value)

    private def inputsValue(inputs: List[TxInInfo]): Value =
        countValue(inputs)(_.resolved.value)

    override def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val Config(owner, recipient) = datum.get.to[Config]
        val contract = tx.findOwnInput(ownRef).get.resolved
        val (contractInputs, contractOutputs) = lookupTx(tx, contract.address.credential)
        val balance = contract.value

        val action = redeemer.to[Action]
        action match
            case Action.Deposit(amount) =>
                require(tx.signatories.contains(owner), "Deposit must be signed by owner")
                // eliminate double satisfaction by ensuring exactly one contract input and one output
                require(contractInputs.size == BigInt(1), "Contract output missing")
                require(contractOutputs.size == BigInt(1), "Contract output missing")
                val contractOutput = contractOutputs.head
                require(
                  contractOutput.value === balance + amount,
                  "Contract has received incorrect amount"
                )
                val expectedDatum = OutputDatum.OutputDatum(datum.get)
                require(contractOutput.datum === expectedDatum, "Output datum changed")
            case Action.Withdraw(withdraw) =>
                require(tx.signatories.contains(recipient), "Withdraw must be signed by recipient")
                require(contractInputs.size == BigInt(1), "Contract output missing")
                if withdraw === balance then
                    // if withdrawing all, there should be no contract output
                    require(contractOutputs.isEmpty, "Contract output not empty")
                else if (balance - withdraw).isPositive then
                    // eliminate double satisfaction by ensuring exactly one contract input and one output
                    require(contractOutputs.size == BigInt(1), "Contract output missing")
                    val contractOutput = contractOutputs.head
                    require(
                      contractOutput.value === balance - withdraw,
                      "Contract balance is incorrect"
                    )
                    val expectedDatum = OutputDatum.OutputDatum(datum.get)
                    require(contractOutput.datum === expectedDatum, "Output datum changed")
                else fail("Withdraw exceeds balance")
    }
}
