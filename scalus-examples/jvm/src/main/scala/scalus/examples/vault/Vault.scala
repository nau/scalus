package scalus.examples.vault

import scalus.builtin.Data.{FromData, ToData}
import scalus.builtin.{ByteString, Data}
import scalus.examples.vault.Redeemer.{Cancel, Deposit, FinalizeWithdrawal, InitiateWithdrawal}
import scalus.ledger.api
import scalus.ledger.api.v1.Credential.ScriptCredential
import scalus.ledger.api.v1.{Credential, Interval, IntervalBoundType, PosixTime, Value}
import scalus.ledger.api.v2.TxOut
import scalus.ledger.api.v3.{TxInInfo, TxInfo, TxOutRef}
import scalus.ledger.api.{v1, v2}
import scalus.prelude.{===, fail, require, Validator}
import scalus.uplc.Program
import scalus.*

case class Datum(
    owner: ByteString,
    state: State,
    amount: BigInt,
    waitTime: PosixTime,
    finalizationDeadline: PosixTime
) derives FromData,
      ToData

enum Redeemer derives FromData, ToData:
    case Deposit
    case InitiateWithdrawal
    case FinalizeWithdrawal
    case Cancel

enum State derives FromData, ToData:
    case Idle
    case Pending

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
@Compile
object Vault extends Validator {

    inline override def spend(
        d: prelude.Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val datum = d.get.to[Datum]
        redeemer.to[Redeemer] match {
            case Deposit            => deposit(tx, ownRef, datum)
            case InitiateWithdrawal => initiateWithdrawal(tx, ownRef, datum)
            case FinalizeWithdrawal => finalize(tx, ownRef, datum)
            case Cancel             => cancel(tx, ownRef, datum)
        }
    }

    def deposit(tx: TxInfo, ownRef: TxOutRef, datum: Datum) = {
        val ownInput = tx.findOwnInput(ownRef).getOrFail(OwnInputNotFound)

        val out = getVaultOutput(tx, ownRef)
        requireSameOwner(out, datum)
        requireOutputToOwnAddress(ownInput, out, WrongDepositDestination)

        val value = out.value
        require(value.withoutLovelace.isZero, CannotAddTokens)

        require(value.getLovelace > ownInput.resolved.value.getLovelace, AdaNotConserved)
        requireEntireVaultIsSpent(datum, ownInput.resolved)
        val newDatum = getVaultDatum(out)
        require(newDatum.amount == value.getLovelace, VaultAmountChanged)
        require(newDatum.waitTime == datum.waitTime, WaitTimeChanged)
        require(
          newDatum.finalizationDeadline == datum.finalizationDeadline,
          FinalizationDeadlineChanged
        )
    }

    def initiateWithdrawal(tx: TxInfo, ownRef: TxOutRef, datum: Datum) = {
        require(
          datum.state.isIdle,
          WithdrawalAlreadyPending
        )
        val ownInput = tx.findOwnInput(ownRef).getOrFail(OwnInputNotFound)
        val out = getVaultOutput(tx, ownRef)
        requireSameOwner(out, datum)
        requireOutputToOwnAddress(
          ownInput,
          out,
          NotExactlyOneVaultOutput
        )

        val requestTime = tx.validRange.getEarliestTime
        val finalizationDeadline = requestTime + datum.waitTime
        val newDatum = getVaultDatum(out)
        require(newDatum.state.isPending, MustBePending)
        require(
          newDatum.finalizationDeadline == finalizationDeadline,
          IncorrectDatumFinalization
        )
    }

    def finalize(tx: TxInfo, ownRef: TxOutRef, datum: Datum) = {
        require(datum.state.isPending, ContractMustBePending)
        require(tx.validRange.isAfter(datum.finalizationDeadline), DeadlineNotPassed)
        val ownInput = tx.findOwnInput(ownRef).getOrFail(OwnInputNotFound)
        requireEntireVaultIsSpent(datum, ownInput.resolved)

        val scriptOutputs = tx.outputs.filter(out =>
            out.address.credential === ownInput.resolved.address.credential
        )
        require(scriptOutputs.size == BigInt(0), WithdrawalsMustNotSendBackToVault)
        val ownerOutputs =
            tx.outputs.filter(out => addressEquals(out.address.credential, datum.owner))
        require(ownerOutputs.size > BigInt(0), WrongAddressWithdrawal)
        val totalToOwner =
            ownerOutputs.foldLeft(BigInt(0))((acc, out) => acc + out.value.getLovelace)
        require(totalToOwner >= datum.amount, VaultAmountChanged)
    }

    def cancel(tx: TxInfo, ownRef: TxOutRef, datum: Datum) = {
        val out = getVaultOutput(tx, ownRef)
        requireSameOwner(out, datum)
        val vaultDatum = getVaultDatum(out)
        require(vaultDatum.amount == datum.amount, VaultAmountChanged)
        require(
          out.value.getLovelace == datum.amount,
          WrongOutputAmount
        )
        require(vaultDatum.state.isIdle, StateNotIdle)
        require(vaultDatum.waitTime == datum.waitTime, WaitTimeChanged)
    }

    def requireEntireVaultIsSpent(datum: Datum, output: TxOut) = {
        val amountToSpend = datum.amount
        val adaSpent = output.value.getLovelace
        require(amountToSpend == adaSpent, AdaLeftover)
    }

    def requireOutputToOwnAddress(ownInput: TxInInfo, out: TxOut, message: String) =
        require(out.address.credential === ownInput.resolved.address.credential, message)

    def getVaultOutput(tx: TxInfo, ownRef: TxOutRef): TxOut = {
        val ownInput = tx.findOwnInput(ownRef).getOrFail("Cannot find own input")
        val scriptOutputs = tx.outputs.filter(out =>
            out.address.credential === ownInput.resolved.address.credential
        )
        require(scriptOutputs.size == BigInt(1), NotExactlyOneVaultOutput)
        scriptOutputs.head
    }

    def getVaultDatum(vaultOutput: TxOut) = vaultOutput.datum match {
        case ledger.api.v2.OutputDatum.OutputDatum(d) => d.to[Datum]
        case _                                        => fail(NoDatumProvided)
    }

    def requireSameOwner(out: TxOut, datum: Datum) =
        out.datum match {
            case scalus.ledger.api.v2.OutputDatum.OutputDatum(newDatum) =>
                require(
                  newDatum.to[Datum].owner == datum.owner,
                  VaultOwnerChanged
                )
            case _ => fail(NoInlineDatum)
        }

    // Errors
    inline val NoDatumExists = "Contract has no datum"
    inline val NoDatumProvided = "Vault transactions must have an inline datum"
    inline val FinalizationDeadlineChanged =
        "Deposit transactions must not change the finalization deadline"
    inline val VaultAmountChanged = "Datum amount must match output lovelace amount"
    inline val CannotAddTokens = "Deposits must only contain ADA"
    inline val AdaNotConserved = "Deposits must add ADA to the vault"
    inline val WrongDepositDestination =
        "Deposit transactions can only be made to the vault"
    inline val NotExactlyOneVaultOutput =
        "Vault transaction must have exactly 1 output to the vault script"
    inline val OwnInputNotFound = "Own input not found"
    inline val IncorrectDatumFinalization =
        "Finalization deadline must be request time plus wait time"
    inline val MustBePending = "Output must have datum with State = Pending"
    inline val WithdrawalAlreadyPending =
        "Cannot withdraw, another withdrawal request is pending"
    inline val WrongAddressWithdrawal =
        "Withdrawal finalization must send funds to the vault owner"
    inline val WithdrawalsMustNotSendBackToVault =
        "Withdrawal finalization must not send funds back to the vault"
    inline val DeadlineNotPassed =
        "Finalization can only happen after the finalization deadline"
    inline val ContractMustBePending = "Contract must be Pending"
    inline val WrongOutputAmount = "Cancel transactions must not change the vault amount"
    inline val WaitTimeChanged = "Wait time must remain the same"
    inline val StateNotIdle = "Idle transactions must change the vault state to Idle"
    inline val NoInlineDatum = "Vault transactions must have an inline datum"
    inline val VaultOwnerChanged = "Vault transactions cannot change the vault owner"
    inline val AdaLeftover = "Must spend entire vault"

    def addressEquals(left: Credential, right: ByteString) = {
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

    extension (i: Interval) {
        def getEarliestTime: PosixTime = i.from.boundType match {
            case IntervalBoundType.Finite(t) => t
            case _                           => BigInt(0)
        }

        def isAfter(timePoint: PosixTime): Boolean = i.from.boundType match {
            case IntervalBoundType.Finite(time) => timePoint < time
            case _                              => false
        }
    }
}

object VaultContract:
    inline def compiled(using options: scalus.Compiler.Options) =
        scalus.Compiler.compileWithOptions(options, Vault.validate)

    given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false
    )

    lazy val script: Program = createScript

    def createScript: Program = {
        val sir = compiled
        sir.toUplc().plutusV3
    }
