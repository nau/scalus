package scalus.examples

import scalus.Compile
import scalus.builtin.{Data, FromData, ToData}
import scalus.ledger.api.v1.PubKeyHash
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

    override def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val Datum(owner, recipient) = datum.get.to[Datum]
        redeemer.to[Redeemer] match {
            case Redeemer.Deposit(deposit) =>
                require(tx.signatories.contains(owner), "Not signed by owner")

            case Redeemer.Withdraw(withdraw) =>
                require(tx.signatories.contains(recipient), "Not signed by recipient")
            // todo check amount
        }
    }
}
