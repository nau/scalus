package scalus.examples

import scalus.Compile
import scalus.prelude.*
import scalus.builtin.*
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*

/** https://github.com/blockchain-unica/rosetta-smart-contracts/tree/main/contracts/simple_transfer
  */
@Compile
object SimpleTransfer extends Validator {

    case class Datum(
        owner: PubKeyHash,
        recipient: PubKeyHash
    ) derives ToData,
          FromData

    enum Redeemer derives ToData, FromData {
        case Deposit(amount: Lovelace)
        case Withdraw(amount: Lovelace)
    }

    private def lookupTx(tx: TxInfo, cred: Credential): (List[TxInInfo], List[TxOut]) = (
      tx.inputs.filter(_.resolved.address.credential === cred),
      tx.outputs.filter(_.address.credential === cred)
    )

    private def countAda[T](a: List[T])(f: T => Lovelace): Lovelace =
        a.map(f).foldLeft(BigInt(0))(_ + _)

    private def outputsAda(outputs: List[TxOut]): Lovelace =
        countAda(outputs)(_.value.getLovelace)

    private def inputsAda(inputs: List[TxInInfo]): Lovelace =
        countAda(inputs)(_.resolved.value.getLovelace)

    override def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val contract = tx.inputs.find(_.outRef === ownRef).get.resolved
        val balance = contract.value.getLovelace
        val (contractInputs, contractOutputs) = lookupTx(tx, contract.address.credential)

        val Datum(owner, recipient) = datum.get.to[Datum]
        val (recipientInputs, recipientOutputs) =
            lookupTx(tx, Credential.PubKeyCredential(recipient))
        val (ownerInputs, ownerOutputs) = lookupTx(tx, Credential.PubKeyCredential(owner))

        require(balance === inputsAda(contractInputs), "Invalid contract balance")
        require(!contractOutputs.isEmpty, "Contract output empty")

        val outputDatum = OutputDatum.OutputDatum(datum.get)
        require(contractOutputs.forall(_.datum === outputDatum), "Output datum invalid")

        redeemer.to[Redeemer] match {
            case Redeemer.Deposit(deposit) =>
                require(deposit >= 0, "Negative amount")
                require(tx.signatories.contains(owner), "Deposit must be signed by owner")
                require(!ownerOutputs.isEmpty, "Deposit must have owner outputs")
                require(
                  outputsAda(contractOutputs) === balance + deposit,
                  "Contract has received incorrect amount"
                )

            case Redeemer.Withdraw(withdraw) =>
                require(withdraw >= 0, "Negative amount")
                require(tx.signatories.contains(recipient), "Withdraw must be signed by recipient")
                require(balance >= withdraw, "Withdraw exceeds balance")
                require(!recipientOutputs.isEmpty, "Withdraw must have recipient outputs")
                require(
                  outputsAda(contractOutputs) === balance - withdraw,
                  "Contract balance is incorrect"
                )
                require(
                  outputsAda(recipientOutputs) === inputsAda(recipientInputs) + withdraw - tx.fee,
                  "Recipient is receiving incorrect amount"
                )
        }
    }
}
