package scalus.cardano.ledger
package utils

import scala.util.boundary
import scala.util.boundary.break

object AllNeededScriptHashes {
    def allNeededScriptHashes(
        utxo: Utxo,
        transaction: Transaction
    ): Either[Throwable, Set[ScriptHash]] = boundary {
        val result =
            neededInputScriptHashesView(utxo, transaction)
                .map {
                    case Right(scriptHash) => scriptHash
                    case Left(error)       => break(Left(error))
                } ++
                neededMintScriptHashes(transaction) ++
                neededVotingProceduresScriptHashesView(transaction) ++
                neededWithdrawalScriptHashesView(transaction) ++
                neededProposalProcedureScriptHashesView(transaction) ++
                neededCertificateScriptHashesView(transaction)

        Right(result.toSet)
    }

    private def neededInputScriptHashesView(
        utxo: Utxo,
        transaction: Transaction
    ): scala.collection.View[Either[Throwable, ScriptHash]] = {
        val inputs = transaction.body.value.inputs
        val transactionId = transaction.id

        for
            (input, index) <- inputs.view.zipWithIndex
            result <- utxo.get(input) match
                case Some(output) => output.address.scriptHash.map(Right(_))
                // This check allows to be an order independent in the sequence of validation rules
                case None =>
                    Some(
                      Left(
                        IllegalArgumentException(
                          s"Missing input $input with index $index in UTxO state for transactionId $transactionId"
                        )
                      )
                    )
        yield result
    }

    private def neededMintScriptHashes(
        transaction: Transaction
    ): Set[PolicyId] = transaction.body.value.mint.getOrElse(Map.empty).keySet

    private def neededVotingProceduresScriptHashesView(
        transaction: Transaction
    ): scala.collection.View[ScriptHash] = {
        val votingProcedures =
            transaction.body.value.votingProcedures.map(_.procedures).getOrElse(Map.empty)

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

    private def neededWithdrawalScriptHashesView(
        transaction: Transaction
    ): scala.collection.View[ScriptHash] = {
        val withdrawals = transaction.body.value.withdrawals.map(_.withdrawals).getOrElse(Map.empty)

        for
            rewardAccount <- withdrawals.keySet.view
            scriptHash <- rewardAccount.address.scriptHash
        yield scriptHash
    }

    private def neededProposalProcedureScriptHashesView(
        transaction: Transaction
    ): scala.collection.View[ScriptHash] = {
        val proposalProcedures = transaction.body.value.proposalProcedures

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

    private def neededCertificateScriptHashesView(
        transaction: Transaction
    ): scala.collection.View[ScriptHash] = {
        val certificates = transaction.body.value.certificates

        def extractScriptHash(credential: Credential): Option[ScriptHash] = {
            credential match
                case _: Credential.KeyHash             => None
                case Credential.ScriptHash(scriptHash) => Some(scriptHash)
        }

        for
            certificate <- certificates.view
            scriptHash <- certificate match
                case cert: Certificate.StakeRegistration   => extractScriptHash(cert.credential)
                case cert: Certificate.StakeDeregistration => extractScriptHash(cert.credential)
                case cert: Certificate.StakeDelegation     => extractScriptHash(cert.credential)
                case _: Certificate.PoolRegistration       => None
                case _: Certificate.PoolRetirement         => None
                case cert: Certificate.RegCert =>
                    if cert.coin > Coin.zero then extractScriptHash(cert.credential)
                    else None // No witness needed for zero deposit
                case cert: Certificate.UnregCert             => extractScriptHash(cert.credential)
                case cert: Certificate.VoteDelegCert         => extractScriptHash(cert.credential)
                case cert: Certificate.StakeVoteDelegCert    => extractScriptHash(cert.credential)
                case cert: Certificate.StakeRegDelegCert     => extractScriptHash(cert.credential)
                case cert: Certificate.VoteRegDelegCert      => extractScriptHash(cert.credential)
                case cert: Certificate.StakeVoteRegDelegCert => extractScriptHash(cert.credential)
                case cert: Certificate.AuthCommitteeHotCert =>
                    extractScriptHash(cert.committeeColdCredential)
                case cert: Certificate.ResignCommitteeColdCert =>
                    extractScriptHash(cert.committeeColdCredential)
                case cert: Certificate.RegDRepCert    => extractScriptHash(cert.drepCredential)
                case cert: Certificate.UnregDRepCert  => extractScriptHash(cert.drepCredential)
                case cert: Certificate.UpdateDRepCert => extractScriptHash(cert.drepCredential)
        yield scriptHash
    }
}
