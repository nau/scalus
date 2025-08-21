package scalus.cardano.ledger
package utils

import scala.collection.View
import scala.util.boundary
import scala.util.boundary.break

object AllNeededScripts {
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

    def allNeededScriptHashesWithIndexAndTag(
        transaction: Transaction,
        utxo: UTxO
    ): Either[TransactionException.BadInputsUTxOException, Set[(ScriptHash, Int, RedeemerTag)]] = {
        allNeededScriptHashesWithIndexAndTagView(transaction, utxo).map(_.toSet)
    }

    def allNeededScriptHashesWithIndexAndTagView(
        transaction: Transaction,
        utxo: UTxO
    ): Either[TransactionException.BadInputsUTxOException, View[(ScriptHash, Int, RedeemerTag)]] = {
        for allNeededInputsScriptHashesWithIndex <- allNeededInputsScriptHashesWithIndex(
              transaction,
              utxo
            )
        yield allNeededInputsScriptHashesWithIndex.view.map(data =>
            (data._1, data._2, RedeemerTag.Spend)
        ) ++
            allNeededMintScriptHashesWithIndex(transaction).view.map(data =>
                (data._1, data._2, RedeemerTag.Mint)
            ) ++
            allNeededVotingProceduresScriptHashesWithIndexView(transaction).map(data =>
                (data._1, data._2, RedeemerTag.Voting)
            ) ++
            allNeededWithdrawalsScriptHashesWithIndexView(transaction).map(data =>
                (data._1, data._2, RedeemerTag.Reward)
            ) ++
            allNeededProposalProceduresScriptHashesWithIndexView(transaction).map(data =>
                (data._1, data._2, RedeemerTag.Proposing)
            ) ++
            allNeededCertificatesScriptHashesWithIndexView(transaction).map(data =>
                (data._1, data._2, RedeemerTag.Cert)
            )
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

    def allNeededInputsScriptHashesWithIndex(
        transaction: Transaction,
        utxo: UTxO
    ): Either[TransactionException.BadInputsUTxOException, Set[(ScriptHash, Int)]] = boundary {
        val transactionId = transaction.id
        val inputsWithIndex = transaction.body.value.inputs.toArray.sorted.view.zipWithIndex

        val result = for
            (input, index) <- inputsWithIndex
            scriptHash <- utxo.get(input) match
                case Some(output) => output.address.scriptHash
                // This check allows to be an order independent in the sequence of validation rules
                case None => break(Left(TransactionException.BadInputsUTxOException(transactionId)))
        yield (scriptHash, index)
        Right(result.toSet)
    }

    def allNeededMintScriptHashes(
        transaction: Transaction
    ): Set[PolicyId] = transaction.body.value.mint.getOrElse(MultiAsset.empty).assets.keySet

    def allNeededMintScriptHashesWithIndex(
        transaction: Transaction
    ): Set[(PolicyId, Int)] = allNeededMintScriptHashes(transaction).zipWithIndex

    def allNeededVotingProceduresScriptHashes(
        transaction: Transaction
    ): Set[ScriptHash] = {
        allNeededVotingProceduresScriptHashesView(transaction).toSet
    }

    def allNeededVotingProceduresScriptHashesView(
        transaction: Transaction
    ): View[ScriptHash] = {
        allNeededVotingProceduresScriptHashesWithIndexView(transaction).map(_._1)
    }

    def allNeededVotingProceduresScriptHashesWithIndex(
        transaction: Transaction
    ): Set[(ScriptHash, Int)] = {
        allNeededVotingProceduresScriptHashesWithIndexView(transaction).toSet
    }

    def allNeededVotingProceduresScriptHashesWithIndexView(
        transaction: Transaction
    ): View[(ScriptHash, Int)] = {
        // FIXME: consider using SortedMap
        val votersWithIndex = transaction.body.value.votingProcedures
            .map(_.procedures)
            .getOrElse(Map.empty)
            .keySet
            .toArray
            .sorted
            .view
            .zipWithIndex

        for
            (voter, index) <- votersWithIndex
            scriptHash <- voter match
                case _: Voter.ConstitutionalCommitteeHotKey             => None
                case _: Voter.StakingPoolKey                            => None
                case _: Voter.DRepKey                                   => None
                case Voter.ConstitutionalCommitteeHotScript(scriptHash) => Some(scriptHash)
                case Voter.DRepScript(scriptHash)                       => Some(scriptHash)
        yield (scriptHash, index)
    }

    def allNeededWithdrawalsScriptHashes(
        transaction: Transaction
    ): Set[ScriptHash] = {
        allNeededWithdrawalsScriptHashesView(transaction).toSet
    }

    def allNeededWithdrawalsScriptHashesView(
        transaction: Transaction
    ): View[ScriptHash] = {
        allNeededWithdrawalsScriptHashesWithIndexView(transaction).map(_._1)
    }

    def allNeededWithdrawalsScriptHashesWithIndex(
        transaction: Transaction
    ): Set[(ScriptHash, Int)] = {
        allNeededWithdrawalsScriptHashesWithIndexView(transaction).toSet
    }

    def allNeededWithdrawalsScriptHashesWithIndexView(
        transaction: Transaction
    ): View[(ScriptHash, Int)] = {
        val rewardAccountsWithIndex = transaction.body.value.withdrawals
            .map(_.withdrawals)
            .getOrElse(Map.empty)
            .keySet
            .toArray
//            .sorted
            .view
            .zipWithIndex

        for
            (rewardAccount, index) <- rewardAccountsWithIndex
            scriptHash <- rewardAccount.address.scriptHash
        yield (scriptHash, index)
    }

    def allNeededProposalProceduresScriptHashes(
        transaction: Transaction
    ): Set[ScriptHash] = {
        allNeededProposalProceduresScriptHashesView(transaction).toSet
    }

    def allNeededProposalProceduresScriptHashesView(
        transaction: Transaction
    ): View[ScriptHash] = {
        allNeededProposalProceduresScriptHashesWithIndexView(transaction).map(_._1)
    }

    def allNeededProposalProceduresScriptHashesWithIndex(
        transaction: Transaction
    ): Set[(ScriptHash, Int)] = {
        allNeededProposalProceduresScriptHashesWithIndexView(transaction).toSet
    }

    def allNeededProposalProceduresScriptHashesWithIndexView(
        transaction: Transaction
    ): View[(ScriptHash, Int)] = {
        val govActionsWithIndex = transaction.body.value.proposalProcedures.toArray
//            .sortBy(_.rewardAccount)
            .view
            .map(_.govAction)
            .zipWithIndex

        for
            (govAction, index) <- govActionsWithIndex
            scriptHash <- govAction match
                case parameterChange: GovAction.ParameterChange => parameterChange.policyHash
                case treasuryWithdrawals: GovAction.TreasuryWithdrawals =>
                    treasuryWithdrawals.policyHash
                case _: GovAction.HardForkInitiation => None
                case _: GovAction.NoConfidence       => None
                case _: GovAction.UpdateCommittee    => None
                case _: GovAction.NewConstitution    => None
                case GovAction.InfoAction            => None
        yield (scriptHash, index)
    }

    def allNeededCertificatesScriptHashes(
        transaction: Transaction
    ): Set[ScriptHash] = {
        allNeededCertificatesScriptHashesView(transaction).toSet
    }

    def allNeededCertificatesScriptHashesView(
        transaction: Transaction
    ): View[ScriptHash] = {
        allNeededCertificatesScriptHashesWithIndexView(transaction).map(_._1)
    }

    def allNeededCertificatesScriptHashesWithIndex(
        transaction: Transaction
    ): Set[(ScriptHash, Int)] = {
        allNeededCertificatesScriptHashesWithIndexView(transaction).toSet
    }

    def allNeededCertificatesScriptHashesWithIndexView(
        transaction: Transaction
    ): View[(ScriptHash, Int)] = {
        // FIXME: should be sorted
        val certificatesWithIndex =
            transaction.body.value.certificates.toIndexedSeq.view.zipWithIndex
        for
            (certificate, index) <- certificatesWithIndex
            scriptHash <- getNeededCertificateScriptHashOption(certificate)
        yield (scriptHash, index)
    }

    def getNeededCertificateScriptHashOption(certificate: Certificate): Option[ScriptHash] = {
        certificate match
            case cert: Certificate.StakeDelegation => cert.credential.scriptHashOption
            case _: Certificate.PoolRegistration   => None
            case _: Certificate.PoolRetirement     => None
            case cert: Certificate.RegCert =>
                if cert.coin.nonEmpty then cert.credential.scriptHashOption else None
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
