package scalus.cardano.ledger
package rules

import scalus.cardano.address.{Address, ShelleyPaymentPart, StakePayload}
import scala.util.boundary
import scala.util.boundary.break

// It's Shelley.validateMissingScripts in cardano-ledger
object MissingScriptsValidator extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = boundary {
        val requiredScripts = allRequiredScripts(state, event).map {
            case Right(scriptHash) => scriptHash
            case Left(error)       => break(failure(error))
        }.toSet

        ???
    }

    private[this] def allRequiredScripts(
        state: State,
        event: Event
    ): scala.collection.View[Either[Error, ScriptHash]] = {
        val inputScripts = requiredInputScripts(state, event)
        val collateralInputScripts = requiredCollateralInputScripts(state, event)
        val mintScripts = requiredMintScripts(event).map(Right(_): Either[Error, ScriptHash])
        val votingProceduresScripts =
            requiredVotingProceduresScripts(event).map(Right(_): Either[Error, ScriptHash])
        val withdrawalScripts =
            requiredWithdrawalScripts(event).map(Right(_): Either[Error, ScriptHash])
        val proposalProcedureScripts =
            requiredProposalProcedureScripts(event).map(Right(_): Either[Error, ScriptHash])
        val certificateScripts =
            requiredCertificateScripts(event).map(Right(_): Either[Error, ScriptHash])

        inputScripts ++ collateralInputScripts ++
            mintScripts ++ votingProceduresScripts ++
            withdrawalScripts ++ proposalProcedureScripts ++
            certificateScripts
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
            scriptHash <- extractScriptHash(rewardAccount.address)
        yield scriptHash
    }

    private[this] def requiredProposalProcedureScripts(
        event: Event
    ): scala.collection.View[ScriptHash] = {
        val proposalProcedures = event.body.value.proposalProcedures

        for
            proposalProcedure <- proposalProcedures.view
            scriptHash <- proposalProcedure.govAction match
                case parameterChange: GovAction.ParameterChange => parameterChange.policyHash
                case treasuryWithdrawals: GovAction.TreasuryWithdrawals =>
                    treasuryWithdrawals.policyHash
                case _: GovAction.HardForkInitiation => None
                case _: GovAction.NoConfidence       => None
                case _: GovAction.UpdateCommittee    => None
                case _: GovAction.NewConstitution    => None
                case GovAction.InfoAction            => None
        yield scriptHash
    }

    private[this] def requiredCertificateScripts(
        event: Event
    ): scala.collection.View[ScriptHash] = {
        val certificates = event.body.value.certificates

        def extractScriptHash(credential: Credential): Option[ScriptHash] = {
            credential match
                case _: Credential.KeyHash             => None
                case Credential.ScriptHash(scriptHash) => Some(scriptHash)
        }

        for
            certificate <- certificates.view
            scriptHash <- certificate match
                case Certificate.StakeRegistration(credential)   => None
                case Certificate.StakeDeregistration(credential) => extractScriptHash(credential)
                case Certificate.StakeDelegation(credential, _)  => extractScriptHash(credential)
                case certificate: Certificate.PoolRegistration =>
                    extractScriptHash(Credential.KeyHash(certificate.operator))
                case Certificate.PoolRetirement(poolKeyHash, _) =>
                    extractScriptHash(Credential.KeyHash(poolKeyHash.asInstanceOf[AddrKeyHash]))
                case Certificate.RegCert(credential, deposit) =>
                    if deposit > Coin.zero then extractScriptHash(credential)
                    else None // No witness needed for zero deposit
                case Certificate.UnregCert(credential, _)     => extractScriptHash(credential)
                case Certificate.VoteDelegCert(credential, _) => extractScriptHash(credential)
                case Certificate.StakeVoteDelegCert(credential, _, _) =>
                    extractScriptHash(credential)
                case Certificate.StakeRegDelegCert(credential, _, _) =>
                    extractScriptHash(credential)
                case Certificate.VoteRegDelegCert(credential, drep, coin) =>
                    extractScriptHash(credential)
                case Certificate.StakeVoteRegDelegCert(credential, _, _, _) =>
                    extractScriptHash(credential)
                case Certificate.AuthCommitteeHotCert(committeeColdCredential, _) =>
                    extractScriptHash(committeeColdCredential)
                case Certificate.ResignCommitteeColdCert(committeeColdCredential, _) =>
                    extractScriptHash(committeeColdCredential)
                case Certificate.RegDRepCert(drepCredential, _, _) =>
                    extractScriptHash(drepCredential)
                case Certificate.UnregDRepCert(drepCredential, _) =>
                    extractScriptHash(drepCredential)
                case Certificate.UpdateDRepCert(drepCredential, _) =>
                    extractScriptHash(drepCredential)
        yield scriptHash
    }

    // TODO add bootstrap witnesses validation
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
                    // This check allows to be an order independent in the sequence of validation rules
                    Some(Left(missingInputError(transactionId, input, index)))
        yield result
    }

//    private[this] def providedTransactionReferenceScripts(
//        inputs: Set[TransactionInput],
//        transactionId: TransactionHash,
//        utxo: Utxo,
//        missingInputError: (TransactionHash, TransactionInput, Int) => IllegalArgumentException
//    ): scala.collection.View[Either[Error, ScriptHash]] = {
//        for
//            (input, index) <- inputs.view.zipWithIndex
//            state.get(input) match
//                case Some(output) =>
//
//                case None =>
//     // If the input is missing, we can't validate it
//                    failure(missingInputError(transactionId, input, index))
//
//    }

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
