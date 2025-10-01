package scalus.examples

import scalus.builtin.{ByteString, Data}
import scalus.builtin.Data.{FromData, ToData}
import scalus.cardano.ledger.Coin
import scalus.examples.Vault.Redeemer.{Cancel, Deposit, Finalize, Withdraw}
import scalus.ledger.api.v1.{Credential, Value}
import scalus.ledger.api.v1.Credential.ScriptCredential
import scalus.ledger.api.v2.{OutputDatum, TxOut}
import scalus.ledger.api.v3.{TxInInfo, TxInfo, TxOutRef}
import scalus.{prelude, Compile}
import scalus.prelude.Validator

/** A contract for keeping funds.
  *
  * Allows withdrawal when 2 conditions are met: a withdrawal request had been issued, and the
  * specified amount of time has elapsed since the request.
  *
  * The withdrawals are allowed only to the address specified in the Datum.
  *
  * Additionally, allows to cancel a withdrawal, and add funds to the vault.
  *
  * Withdrawal requires 2 actions: 1) Send a `Withdraw` request that contains the Datum-matching
  * verification key hash. 2) Send a `Finalize` request after a waiting period to confirm the
  * spending of funds.
  */
// todo: allow key-authorized withdrawals
object Vault extends Validator {

    case class Datum(
        sender: ByteString,
        state: State,
        amount: BigInt
    ) derives FromData,
          ToData

    enum Redeemer derives FromData, ToData:
        case Deposit
        case Withdraw
        case Finalize
        case Cancel

    enum State derives FromData, ToData:
        case Idle
        case Pending

    override def spend(
        d: prelude.Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val datum = d.get.to[Datum]
        redeemer.to[Redeemer] match {
            case Deposit  => deposit(tx, ownRef, datum)
            case Withdraw => withdraw(tx, ownRef, datum)
            case Finalize => finalize(tx, ownRef, datum)
            case Cancel   =>
        }
    }

    private def deposit(tx: TxInfo, ownRef: TxOutRef, datum: Datum) = {
        // todo for now, let's only allow a single output to the script.
        //    later, allow multiple
        require(tx.outputs.size == BigInt(1), "Deposits must contain exactly one output.")

        val ownInput = tx.findOwnInput(ownRef).getOrFail("Own input not found.")
        val out = tx.outputs.head
        requireOutputToOwnAddress(
          ownInput,
          out,
          "Deposit transactions can only be made to the vault."
        )

        val value = out.value
        require(value.withoutLovelace.isZero, "Deposits must only contain ADA.")
        require(
          value.getLovelace > ownInput.resolved.value.getLovelace,
          "Deposits must add ADA to the vault."
        )
        requireEntireVaultIsSpent(datum, out)
    }

    private def withdraw(tx: TxInfo, ownRef: TxOutRef, datum: Datum) = {
        require(
          datum.state == State.Idle,
          "Cannot withdraw, another withdrawal request is pending."
        )
        require(tx.outputs.size == BigInt(1), "Withdrawals transaction must contain 1 output.")
        val out = tx.outputs.head

        val ownInput = tx.findOwnInput(ownRef).getOrFail("Cannot find own input.")
        requireOutputToOwnAddress(
          ownInput,
          out,
          "Withdrawal transactions must have a vault output."
        )
        out.datum match {
            case OutputDatum.OutputDatum(datum) =>
                require(
                  datum.to[Datum].state == State.Pending,
                  "Output must have datum with State = Pending."
                )
            case _ => require(false, "Output must have datum with State = Pending.")
        }
    }

    private def requireEntireVaultIsSpent(datum: Datum, output: TxOut) = {
        val amountToSpend = datum.amount
        val adaSpent = output.value.getLovelace
        require(amountToSpend == adaSpent, "Must spend entire vault.")
    }

    private def requireOutputToOwnAddress(ownInput: TxInInfo, out: TxOut, message: String) =
        require(out.address.credential == ownInput.resolved.address.credential, message)
}
