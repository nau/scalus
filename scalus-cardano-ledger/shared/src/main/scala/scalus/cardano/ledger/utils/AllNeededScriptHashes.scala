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
        for
            certificate <- certificates.toIndexedSeq.view
            scriptHash <- getNeededScriptHashOption(certificate)
        yield scriptHash
    }

    def getNeededScriptHashOption(certificate: Certificate): Option[ScriptHash] = {
        certificate match
            case Certificate.RegCert(credential, None) => credential.scriptHashOption
            case cert: Certificate.StakeDelegation     => cert.credential.scriptHashOption
            case _: Certificate.PoolRegistration       => None
            case _: Certificate.PoolRetirement         => None
            case Certificate.RegCert(credential, Some(coin)) =>
                if coin > Coin.zero then credential.scriptHashOption
                else None // No witness needed for zero deposit
            case cert: Certificate.UnregCert             => cert.credential.scriptHashOption
            case cert: Certificate.VoteDelegCert         => cert.credential.scriptHashOption
            case cert: Certificate.StakeVoteDelegCert    => cert.credential.scriptHashOption
            case cert: Certificate.StakeRegDelegCert     => cert.credential.scriptHashOption
            case cert: Certificate.VoteRegDelegCert      => cert.credential.scriptHashOption
            case cert: Certificate.StakeVoteRegDelegCert => cert.credential.scriptHashOption
            case cert: Certificate.AuthCommitteeHotCert =>
                cert.committeeColdCredential.scriptHashOption
            case cert: Certificate.ResignCommitteeColdCert =>
                cert.committeeColdCredential.scriptHashOption
            case cert: Certificate.RegDRepCert    => cert.drepCredential.scriptHashOption
            case cert: Certificate.UnregDRepCert  => cert.drepCredential.scriptHashOption
            case cert: Certificate.UpdateDRepCert => cert.drepCredential.scriptHashOption
    }
}
