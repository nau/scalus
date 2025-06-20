package scalus.cardano.ledger
package rules

import scalus.cardano.address.{Address, ShelleyPaymentPart, StakePayload}
import scala.util.boundary
import scala.util.boundary.break

// It's babbageMissingScripts in cardano-ledger
object MissingScriptsValidator extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = {
        for
            requiredScripts <- allRequiredScripts(state, event)
            referenceScripts <- allReferenceScripts(state, event)
            requiredScriptsNonRefs = requiredScripts.diff(referenceScripts)
            providedScripts = allProvidedScripts(event)
            _ <-
                val missing = requiredScriptsNonRefs.diff(providedScripts)
                if missing.isEmpty then success
                else
                    failure(
                      IllegalArgumentException(
                        s"Missing scripts: $missing transactionId ${event.id}"
                      )
                    )
            _ <-
                val extra = providedScripts.diff(requiredScriptsNonRefs)
                if extra.isEmpty then success
                else
                    failure(
                      IllegalArgumentException(
                        s"Extra scripts: $extra transactionId ${event.id}"
                      )
                    )
        yield ()
    }

    private def allRequiredScripts(
        state: State,
        event: Event
    ): Either[Error, Set[ScriptHash]] = boundary {
        val result =
            (requiredInputScripts(state, event) ++ requiredCollateralInputScripts(state, event))
                .map {
                    case Right(scriptHash) => scriptHash
                    case Left(error)       => break(Left(error))
                } ++
                requiredMintScripts(event) ++
                requiredVotingProceduresScripts(event) ++
                requiredWithdrawalScripts(event) ++
                requiredProposalProcedureScripts(event) ++
                requiredCertificateScripts(event)

        Right(result.toSet)
    }

    private def allReferenceScripts(
        state: State,
        event: Event
    ): Either[Error, Set[ScriptHash]] = boundary {
        val result = (
          inputReferenceScripts(state, event) ++ referenceInputReferenceScripts(state, event)
        ).map {
            case Right(scriptHash) => scriptHash
            case Left(error)       => break(Left(error))
        }

        Right(result.toSet)
    }

    private def allProvidedScripts(event: Event): Set[ScriptHash] = {
        val witnessSet = event.witnessSet

        (
          witnessSet.nativeScripts.view.map(Script.nativeScriptHash) ++
              witnessSet.plutusV1Scripts.map(Script.plutusV1ScriptHash) ++
              witnessSet.plutusV2Scripts.map(Script.plutusV2ScriptHash) ++
              witnessSet.plutusV3Scripts.map(Script.plutusV3ScriptHash)
        ).toSet
    }

    private def requiredInputScripts(
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

    private def requiredCollateralInputScripts(
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

    private def requiredMintScripts(
        event: Event
    ): Set[PolicyId] = event.body.value.mint.getOrElse(Map.empty).keySet

    private def requiredVotingProceduresScripts(
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

    private def requiredWithdrawalScripts(
        event: Event
    ): scala.collection.View[ScriptHash] = {
        val withdrawals = event.body.value.withdrawals.map(_.withdrawals).getOrElse(Map.empty)

        for
            rewardAccount <- withdrawals.keySet.view
            scriptHash <- extractScriptHash(rewardAccount.address)
        yield scriptHash
    }

    private def requiredProposalProcedureScripts(
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

    private def requiredCertificateScripts(
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

    private def inputReferenceScripts(
        state: State,
        event: Event
    ): scala.collection.View[Either[Error, ScriptHash]] = {
        transactionReferenceScripts(
          event.body.value.inputs,
          event.id,
          state.utxo,
          (transactionId, input, index) =>
              IllegalArgumentException(
                s"Missing input $input with index $index in UTxO state for transactionId $transactionId"
              )
        )
    }

    private def referenceInputReferenceScripts(
        state: State,
        event: Event
    ): scala.collection.View[Either[Error, ScriptHash]] = {
        transactionReferenceScripts(
          event.body.value.referenceInputs,
          event.id,
          state.utxo,
          (transactionId, input, index) =>
              IllegalArgumentException(
                s"Missing reference input $input with index $index in UTxO state for transactionId $transactionId"
              )
        )
    }

    private def requiredTransactionInputScripts(
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
                // This check allows to be an order independent in the sequence of validation rules
                case None =>
                    Some(Left(missingInputError(transactionId, input, index)))
        yield result
    }

    private def transactionReferenceScripts(
        inputs: Set[TransactionInput],
        transactionId: TransactionHash,
        utxo: Utxo,
        missingInputError: (TransactionHash, TransactionInput, Int) => IllegalArgumentException
    ): scala.collection.View[Either[Error, ScriptHash]] = {
        for
            (input, index) <- inputs.view.zipWithIndex
            result <- utxo.get(input) match
                case Some(output) =>
                    output match
                        case babbage: TransactionOutput.Babbage =>
                            babbage.scriptRef match
                                case Some(ScriptRef(script)) => Some(Right(script.scriptHash))
                                case None                    => None
                        case _: TransactionOutput.Shelley => None
                // This check allows to be an order independent in the sequence of validation rules
                case None =>
                    Some(Left(missingInputError(transactionId, input, index)))
        yield result
    }

    private def extractScriptHash(
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
