package scalus.cardano.ledger
package utils

import scala.collection.View
import scala.util.boundary
import scala.util.boundary.break

object AllNeededScriptHashes {
    def allNeededScriptHashes(
        transaction: Transaction,
        utxo: UTxO
    ): Either[TransactionException.BadInputsUTxOException, Set[ScriptHash]] = {
        allNeededScriptHashesView(transaction, utxo).map(_.toSet)
    }

    def allNeededScriptHashesView(
        transaction: Transaction,
        utxo: UTxO
    ): Either[TransactionException.BadInputsUTxOException, View[ScriptHash]] = {
        for allNeededInputsScriptHashes <- allNeededInputsScriptHashes(transaction, utxo)
        yield allNeededInputsScriptHashes.view ++
            allNeededMintScriptHashes(transaction).view ++
            allNeededVotingProceduresScriptHashesView(transaction) ++
            allNeededWithdrawalsScriptHashesView(transaction) ++
            allNeededProposalProceduresScriptHashesView(transaction) ++
            allNeededCertificatesScriptHashesView(transaction)
    }

    def allNeededInputsScriptHashes(
        transaction: Transaction,
        utxo: UTxO
    ): Either[TransactionException.BadInputsUTxOException, Set[ScriptHash]] = boundary {
        val transactionId = transaction.id
        val inputs = transaction.body.value.inputs

        val result = for
            input <- inputs
            scriptHash <- utxo.get(input) match
                case Some(output) => output.address.scriptHash
                // This check allows to be an order independent in the sequence of validation rules
                case None => break(Left(TransactionException.BadInputsUTxOException(transactionId)))
        yield scriptHash
        Right(result)
    }

    def allNeededMintScriptHashes(
        transaction: Transaction
    ): Set[PolicyId] = transaction.body.value.mint.getOrElse(MultiAsset.empty).assets.keySet

    def allNeededVotingProceduresScriptHashes(
        transaction: Transaction
    ): Set[ScriptHash] = {
        allNeededVotingProceduresScriptHashesView(transaction).toSet
    }

    def allNeededVotingProceduresScriptHashesView(
        transaction: Transaction
    ): View[ScriptHash] = {
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

    def allNeededWithdrawalsScriptHashes(
        transaction: Transaction
    ): Set[ScriptHash] = {
        allNeededWithdrawalsScriptHashesView(transaction).toSet
    }

    def allNeededWithdrawalsScriptHashesView(
        transaction: Transaction
    ): View[ScriptHash] = {
        val withdrawals = transaction.body.value.withdrawals.map(_.withdrawals).getOrElse(Map.empty)

        for
            rewardAccount <- withdrawals.keySet.view
            scriptHash <- rewardAccount.address.scriptHash
        yield scriptHash
    }

    def allNeededProposalProceduresScriptHashes(
        transaction: Transaction
    ): Set[ScriptHash] = {
        allNeededProposalProceduresScriptHashesView(transaction).toSet
    }

    def allNeededProposalProceduresScriptHashesView(
        transaction: Transaction
    ): View[ScriptHash] = {
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

    def allNeededCertificatesScriptHashes(
        transaction: Transaction
    ): Set[ScriptHash] = {
        allNeededCertificatesScriptHashesView(transaction).toSet
    }

    def allNeededCertificatesScriptHashesView(
        transaction: Transaction
    ): View[ScriptHash] = {
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
