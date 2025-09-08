package scalus.examples

import scalus.Compile
import scalus.builtin.{Data, FromData, ToData}
import scalus.ledger.api.v1.{Credential, PubKeyHash}
import scalus.ledger.api.v3.*
import scalus.prelude.*

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

    override def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val contract = tx.inputs.find(_.outRef === ownRef).get.resolved
        val Datum(owner, recipient) = datum.get.to[Datum]

        val (contractInputs, contractOutputs) = lookupTx(tx, contract.address.credential)
        val (recipientInputs, recipientOutputs) = lookupTx(tx, Credential.PubKeyCredential(owner))
        val (ownerInputs, ownerOutputs) = lookupTx(tx, Credential.PubKeyCredential(recipient))

        redeemer.to[Redeemer] match {
            case Redeemer.Deposit(deposit) =>
                require(tx.signatories.contains(owner), "Not signed by owner")

            case Redeemer.Withdraw(withdraw) =>
                require(tx.signatories.contains(recipient), "Not signed by recipient")
            // todo check amount
        }
    }
}
