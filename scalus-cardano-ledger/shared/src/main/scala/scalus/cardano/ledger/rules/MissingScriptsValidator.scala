package scalus.cardano.ledger
package rules

import scalus.cardano.address.{Address, ShelleyPaymentPart, StakePayload}
import scala.util.boundary
import scala.util.boundary.break

object MissingScriptsValidator extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = {

        ???
    }

    private[this] def allRequiredScripts(state: State, event: Event): Set[ScriptHash] = {
        val transactionId = event.id
        val body = event.body.value
        val utxo = state.utxo

        ???
    }

    private[this] def requiredInputScripts(
        state: State,
        event: Event
    ): scala.collection.View[Either[Error, ScriptHash]] = {
        requiredTransactionInputScripts(
          event.body.value.inputs,
          event.id,
          state.utxo,
          (transactionId, input, index) =>
              IllegalArgumentException(
                s"Missing input $input with index $index in UTxO state for transactionId $transactionId"
              )
        )
    }

    private[this] def requiredCollateralInputScripts(
        state: State,
        event: Event
    ): scala.collection.View[Either[Error, ScriptHash]] = {
        requiredTransactionInputScripts(
          event.body.value.collateralInputs,
          event.id,
          state.utxo,
          (transactionId, input, index) =>
              IllegalArgumentException(
                s"Missing collateral input $input with index $index in UTxO state for transactionId $transactionId"
              )
        )
    }

    private[this] def requiredMintScripts(
        event: Event
    ): Set[PolicyId] = event.body.value.mint.getOrElse(Map.empty).keySet

    private[this] def requiredVotingProceduresScripts(
        event: Event
    ): scala.collection.View[ScriptHash] = {
        val votingProcedures =
            event.body.value.votingProcedures.map(_.procedures).getOrElse(Map.empty)
            
        for
            voter <- votingProcedures.keySet.view
            scriptHash <- voter match
                case _: Voter.ConstitutionalCommitteeHotKey             => None
                case _: Voter.StakingPoolKey                            => None
                case _: Voter.DRepKey                                   => None
                case Voter.ConstitutionalCommitteeHotScript(scriptHash) => Some(scriptHash)
                case Voter.DRepScript(scriptHash)                       => Some(scriptHash)
        yield scriptHash
    }

    private[this] def requiredWithdrawalScripts(
        event: Event
    ): scala.collection.View[ScriptHash] = {
        val withdrawals = event.body.value.withdrawals.map(_.withdrawals).getOrElse(Map.empty)
        
        for
            rewardAccount <- withdrawals.keySet.view
            keyHash <- extractScriptHash(rewardAccount.address)
        yield keyHash
    }

    private[this] def requiredProposalProcedureScripts(
        event: Event
    ): scala.collection.View[ScriptHash] = {
        val proposalProcedures = event.body.value.proposalProcedures

        for
            proposalProcedure <- proposalProcedures.view
            keyHash <- proposalProcedure.govAction match 
                case parameterChange: GovAction.ParameterChange => parameterChange.policyHash
                case treasuryWithdrawals: GovAction.TreasuryWithdrawals => treasuryWithdrawals.policyHash
                case _: GovAction.HardForkInitiation => None
                case _: GovAction.NoConfidence => None
                case _: GovAction.UpdateCommittee => None
                case _: GovAction.NewConstitution => None
                case GovAction.InfoAction => None
        yield keyHash
    }

    private[this] def requiredCertificateScripts(
        event: Event
    ): scala.collection.View[ScriptHash] = {
        val certificates = event.body.value.certificates

        for
            certificate <- certificates.view
            keyHash <- extractCertificateScriptHash(certificate)
        yield keyHash
    }

    private[this] def requiredTransactionInputScripts(
        inputs: Set[TransactionInput],
        transactionId: TransactionHash,
        utxo: Utxo,
        missingInputError: (TransactionHash, TransactionInput, Int) => IllegalArgumentException
    ): scala.collection.View[Either[Error, ScriptHash]] = {
        for
            (input, index) <- inputs.view.zipWithIndex
            result <- utxo.get(input) match
                case Some(output) =>
                    extractScriptHash(output.address) match
                        case Some(scriptHash) => Some(Right(scriptHash))
                        case None             => None
                case None =>
                    // If the input is missing, we can't validate it
                    Some(Left(missingInputError(transactionId, input, index)))
        yield result
    }

    private[this] def extractScriptHash(
        address: Address
    ): Option[ScriptHash] = {
        address match
            case Address.Byron(_) =>
                None // Byron addresses don't have staking credentials
            case Address.Shelley(shelleyAddress) =>
                shelleyAddress.payment match
                    case _: ShelleyPaymentPart.Key       => None
                    case ShelleyPaymentPart.Script(hash) => Some(hash)
            case Address.Stake(stakeAddress) =>
                stakeAddress.payload match
                    case _: StakePayload.Stake     => None
                    case StakePayload.Script(hash) => Some(hash)
    }
}
