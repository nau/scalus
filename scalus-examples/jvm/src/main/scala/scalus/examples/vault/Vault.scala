package scalus.examples.vault

import scalus.builtin.Data.{FromData, ToData}
import scalus.builtin.{ByteString, Data}
import scalus.examples.vault.Vault.Redeemer.{Cancel, Deposit, Finalize, Withdraw}
import scalus.ledger.api
import scalus.ledger.api.v1.Credential.ScriptCredential
import scalus.ledger.api.v1.{Credential, Value}
import scalus.ledger.api.v2.{OutputDatum, TxOut}
import scalus.ledger.api.v3.{TxInInfo, TxInfo, TxOutRef}
import scalus.ledger.api.{v1, v2}
import scalus.prelude.{===, fail, require, Validator}
import scalus.uplc.Program
import scalus.*

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
@Compile
object Vault extends Validator {

    case class Datum(
        owner: ByteString,
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

    inline override def spend(
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
            case Cancel   => cancel(tx, ownRef, datum)
        }
    }

    def deposit(tx: TxInfo, ownRef: TxOutRef, datum: Datum) = {
        val ownInput = tx.findOwnInput(ownRef).getOrFail("Own input not found")

        val scriptOutputs = tx.outputs.filter(out =>
            out.address.credential === ownInput.resolved.address.credential
        )

        require(
          scriptOutputs.size == BigInt(1),
          "Deposit transaction must have exactly 1 output to the vault script"
        )

        val out = scriptOutputs.head
        requireSameOwner(out, datum)
        requireOutputToOwnAddress(
          ownInput,
          out,
          "Deposit transactions can only be made to the vault"
        )

        val value = out.value
        require(value.withoutLovelace.isZero, "Deposits must only contain ADA")
        require(
          value.getLovelace > ownInput.resolved.value.getLovelace,
          "Deposits must add ADA to the vault"
        )
        requireEntireVaultIsSpent(datum, ownInput.resolved)
        out.datum match {
            case api.v2.OutputDatum.OutputDatum(datum) =>
                require(
                  datum.to[Datum].amount == value.getLovelace,
                  "Datum amount must match output lovelace amount"
                )
            case _ =>
                fail("Deposit transaction must have an inline datum with the new amount")
        }
    }

    def withdraw(tx: TxInfo, ownRef: TxOutRef, datum: Datum) = {
        require(
          datum.state.isIdle,
          "Cannot withdraw, another withdrawal request is pending"
        )
        val ownInput = tx.findOwnInput(ownRef).getOrFail("Cannot find own input")

        val scriptOutputs = tx.outputs.filter(out =>
            out.address.credential === ownInput.resolved.address.credential
        )

        require(
          scriptOutputs.size == BigInt(1),
          "Withdrawal transaction must contain exactly 1 output to the vault script"
        )

        val out = scriptOutputs.head
        requireSameOwner(out, datum)
        requireOutputToOwnAddress(
          ownInput,
          out,
          "Withdrawal transactions must have a vault output"
        )
        out.datum match {
            case OutputDatum.OutputDatum(datum) =>
                require(
                  datum.to[Datum].state.isPending,
                  "Output must have datum with State = Pending"
                )
            case _ => fail("Output must have datum with State = Pending")
        }
    }

    def finalize(tx: TxInfo, ownRef: TxOutRef, datum: Datum) = {
        val ownInput = tx.findOwnInput(ownRef).getOrFail("Cannot find own input")

        // We can only finalize pending vaults.
        ownInput.resolved.datum match {
            case OutputDatum.OutputDatum(datum) => require(datum.to[Datum].state.isPending)
            case _                              => fail("Contract has no datum.")
        }

        val scriptOutputs = tx.outputs.filter(out =>
            out.address.credential === ownInput.resolved.address.credential
        )

        // Finalize should close the vault - no outputs back to the script
        require(
          scriptOutputs.size == BigInt(0),
          "Withdrawal finalization must not send funds back to the vault"
        )

        // Ensure there's at least one output to the owner address
        val ownerOutputs =
            tx.outputs.filter(out => addressEquals(out.address.credential, datum.owner))
        require(
          ownerOutputs.size > BigInt(0),
          "Withdrawal finalization must send funds to the vault owner"
        )
    }

    def cancel(tx: TxInfo, ownRef: TxOutRef, datum: Datum) = {
        val ownInput = tx.findOwnInput(ownRef).getOrFail("Cannot find own input")

        // Filter outputs to only those going to the script address
        val scriptOutputs = tx.outputs.filter(out =>
            out.address.credential === ownInput.resolved.address.credential
        )

        require(
          scriptOutputs.size == BigInt(1),
          "Cancel transaction must have exactly 1 output to the vault script"
        )

        val out = scriptOutputs.head
        requireSameOwner(out, datum)
        out.datum match {
            case ledger.api.v2.OutputDatum.OutputDatum(d) =>
                val newDatum = d.to[Datum]
                require(
                  newDatum.amount == datum.amount,
                  "Cancel transactions must not change the vault amount"
                )
                require(
                  out.value.getLovelace == datum.amount,
                  "Cancel transactions must not change the vault amount"
                )
                require(
                  newDatum.state.isIdle,
                  "Idle transactions must change the vault state to Idle"
                )
            case _ =>
                fail(
                  "Cancel transactions must have an inline datum with the correct state and amount"
                )
        }
    }

    private def requireEntireVaultIsSpent(datum: Datum, output: TxOut) = {
        val amountToSpend = datum.amount
        val adaSpent = output.value.getLovelace
        require(amountToSpend == adaSpent, "Must spend entire vault")
    }

    private def requireOutputToOwnAddress(ownInput: TxInInfo, out: TxOut, message: String) =
        require(out.address.credential === ownInput.resolved.address.credential, message)

    private def requireSameOwner(out: TxOut, datum: Datum) =
        out.datum match {
            case scalus.ledger.api.v2.OutputDatum.OutputDatum(newDatum) =>
                require(
                  newDatum.to[Datum].owner == datum.owner,
                  "Vault transactions cannot change the vault owner"
                )
            case _ => fail("Vault transactions must have an inline datum")
        }

    private def addressEquals(left: Credential, right: ByteString) = {
        left match {
            case v1.Credential.PubKeyCredential(hash) => hash.hash === right
            case v1.Credential.ScriptCredential(hash) => hash === right
        }
    }

    extension (s: State) {
        def isPending: Boolean = s match {
            case State.Idle    => false
            case State.Pending => true
        }

        def isIdle: Boolean = s match {
            case State.Idle    => true
            case State.Pending => false
        }
    }
}

object VaultContract:
    inline def compiled(using options: scalus.Compiler.Options) =
        scalus.Compiler.compileWithOptions(options, Vault.validate)

    inline given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false
    )

    def script: Program =
        compiled
            .toUplc(
              generateErrorTraces = true,
              backend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering
            )
            .plutusV3
