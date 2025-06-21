package scalus.cardano.ledger
package rules
package utils

import scala.util.boundary
import scala.util.boundary.break

trait AllRequiredScriptHashes {
    this: STS =>

    protected def allRequiredScriptHashes(
        state: State,
        event: Event
    ): Either[Error, Set[ScriptHash]] = boundary {
        val result = (
          requiredInputScriptHashesView(state, event) ++
              requiredCollateralInputScriptHashesView(state, event)
        ).map {
            case Right(scriptHash) => scriptHash
            case Left(error)       => break(Left(error))
        } ++
            requiredMintScriptHashes(event) ++
            requiredVotingProceduresScriptHashesView(event) ++
            requiredWithdrawalScriptHashesView(event) ++
            requiredProposalProcedureScriptHashesView(event) ++
            requiredCertificateScriptHashesView(event)

        Right(result.toSet)
    }

    private def requiredInputScriptHashesView(
        state: State,
        event: Event
    ): scala.collection.View[Either[Error, ScriptHash]] = {
        requiredTransactionInputScriptHashesView(
          event.body.value.inputs,
          event.id,
          state.utxo,
          (transactionId, input, index) =>
              IllegalArgumentException(
                s"Missing input $input with index $index in UTxO state for transactionId $transactionId"
              )
        )
    }

    private def requiredCollateralInputScriptHashesView(
        state: State,
        event: Event
    ): scala.collection.View[Either[Error, ScriptHash]] = {
        requiredTransactionInputScriptHashesView(
          event.body.value.collateralInputs,
          event.id,
          state.utxo,
          (transactionId, input, index) =>
              IllegalArgumentException(
                s"Missing collateral input $input with index $index in UTxO state for transactionId $transactionId"
              )
        )
    }

    private def requiredMintScriptHashes(
        event: Event
    ): Set[PolicyId] = event.body.value.mint.getOrElse(Map.empty).keySet

    private def requiredVotingProceduresScriptHashesView(
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

    private def requiredWithdrawalScriptHashesView(
        event: Event
    ): scala.collection.View[ScriptHash] = {
        val withdrawals = event.body.value.withdrawals.map(_.withdrawals).getOrElse(Map.empty)

        for
            rewardAccount <- withdrawals.keySet.view
            scriptHash <- rewardAccount.address.scriptHash
        yield scriptHash
    }

    private def requiredProposalProcedureScriptHashesView(
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

    private def requiredCertificateScriptHashesView(
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

    private def requiredTransactionInputScriptHashesView(
        inputs: Set[TransactionInput],
        transactionId: TransactionHash,
        utxo: Utxo,
        missingInputError: (TransactionHash, TransactionInput, Int) => IllegalArgumentException
    ): scala.collection.View[Either[Error, ScriptHash]] = {
        for
            (input, index) <- inputs.view.zipWithIndex
            result <- utxo.get(input) match
                case Some(output) => output.address.scriptHash.map(Right(_))
                // This check allows to be an order independent in the sequence of validation rules
                case None => Some(Left(missingInputError(transactionId, input, index)))
        yield result
    }
}
