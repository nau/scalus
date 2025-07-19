package scalus.cardano.ledger
package utils

import scala.collection.View
import scala.util.boundary
import scala.util.boundary.break

object AllNeededKeyHashes {
    def allNeededKeyHashes(
        transaction: Transaction,
        utxo: UTxO
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadCollateralInputsUTxOException,
      Set[AddrKeyHash | StakeKeyHash | PoolKeyHash]
    ] = {
        allNeededKeyHashesView(transaction, utxo).map(_.toSet)
    }

    def allNeededKeyHashesView(
        transaction: Transaction,
        utxo: UTxO
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadCollateralInputsUTxOException,
      View[AddrKeyHash | StakeKeyHash | PoolKeyHash]
    ] = {
        for
            allNeededInputsKeyHashes <- allNeededInputsKeyHashes(transaction, utxo)
            allNeededCollateralInputsKeyHashes <- allNeededCollateralInputsKeyHashes(
              transaction,
              utxo
            )
        yield
            allNeededInputsKeyHashes.view ++
                allNeededCollateralInputsKeyHashes.view
            allNeededVotingProceduresKeyHashesView(transaction) ++
                allNeededWithdrawalsKeyHashesView(transaction) ++
                allNeededCertificatesKeyHashesView(transaction) ++
                allNeededRequiredSignersKeyHashes(transaction).view
    }

    def allNeededInputsKeyHashes(
        transaction: Transaction,
        utxo: UTxO
    ): Either[TransactionException.BadInputsUTxOException, Set[AddrKeyHash | StakeKeyHash]] = {
        neededInputsKeyHashes(
          transaction.id,
          transaction.body.value.inputs,
          utxo,
          TransactionException.BadInputsUTxOException(_)
        )
    }

    def allNeededCollateralInputsKeyHashes(
        transaction: Transaction,
        utxo: UTxO
    ): Either[TransactionException.BadCollateralInputsUTxOException, Set[
      AddrKeyHash | StakeKeyHash
    ]] = {
        neededInputsKeyHashes(
          transaction.id,
          transaction.body.value.collateralInputs,
          utxo,
          TransactionException.BadCollateralInputsUTxOException(_)
        )
    }

    def allNeededVotingProceduresKeyHashes(
        transaction: Transaction
    ): Set[AddrKeyHash] = {
        allNeededVotingProceduresKeyHashesView(transaction).toSet
    }

    def allNeededVotingProceduresKeyHashesView(
        transaction: Transaction
    ): View[AddrKeyHash] = {
        val votingProcedures =
            transaction.body.value.votingProcedures.map(_.procedures).getOrElse(Map.empty)

        for
            voter <- votingProcedures.keySet.view
            keyHash <- voter match
                case Voter.ConstitutionalCommitteeHotKey(keyHash) => Some(keyHash)
                case Voter.StakingPoolKey(keyHash)                => Some(keyHash)
                case Voter.DRepKey(keyHash)                       => Some(keyHash)
                case _: Voter.ConstitutionalCommitteeHotScript    => None
                case _: Voter.DRepScript                          => None
        yield keyHash
    }

    def allNeededWithdrawalsKeyHashes(
        transaction: Transaction
    ): Set[AddrKeyHash | StakeKeyHash] = {
        allNeededWithdrawalsKeyHashesView(transaction).toSet
    }

    def allNeededWithdrawalsKeyHashesView(
        transaction: Transaction
    ): View[AddrKeyHash | StakeKeyHash] = {
        val withdrawals = transaction.body.value.withdrawals.map(_.withdrawals).getOrElse(Map.empty)

        for
            rewardAccount <- withdrawals.keySet.view
            keyHash <- rewardAccount.address.keyHash
        yield keyHash
    }

    def allNeededCertificatesKeyHashes(
        transaction: Transaction
    ): Set[AddrKeyHash | PoolKeyHash] = {
        allNeededCertificatesKeyHashesView(transaction).toSet
    }

    def allNeededCertificatesKeyHashesView(
        transaction: Transaction
    ): View[AddrKeyHash | PoolKeyHash] = {
        val certificates = transaction.body.value.certificates
        for
            certificate <- certificates.toIndexedSeq.view
            keyHash: (AddrKeyHash | PoolKeyHash) <- certificate match
                case cert: Certificate.StakeRegistration   => cert.credential.keyHashOption
                case cert: Certificate.StakeDeregistration => cert.credential.keyHashOption
                case cert: Certificate.StakeDelegation     => cert.credential.keyHashOption
                case cert: Certificate.PoolRegistration =>
                    cert.poolOwners.view.concat(Some(cert.operator))
                case cert: Certificate.PoolRetirement => Some(cert.poolKeyHash)
                case cert: Certificate.RegCert =>
                    if cert.coin > Coin.zero then cert.credential.keyHashOption
                    else None // No witness needed for zero deposit
                case cert: Certificate.UnregCert             => cert.credential.keyHashOption
                case cert: Certificate.VoteDelegCert         => cert.credential.keyHashOption
                case cert: Certificate.StakeVoteDelegCert    => cert.credential.keyHashOption
                case cert: Certificate.StakeRegDelegCert     => cert.credential.keyHashOption
                case cert: Certificate.VoteRegDelegCert      => cert.credential.keyHashOption
                case cert: Certificate.StakeVoteRegDelegCert => cert.credential.keyHashOption
                case cert: Certificate.AuthCommitteeHotCert =>
                    cert.committeeColdCredential.keyHashOption
                case cert: Certificate.ResignCommitteeColdCert =>
                    cert.committeeColdCredential.keyHashOption
                case cert: Certificate.RegDRepCert    => cert.drepCredential.keyHashOption
                case cert: Certificate.UnregDRepCert  => cert.drepCredential.keyHashOption
                case cert: Certificate.UpdateDRepCert => cert.drepCredential.keyHashOption
        yield keyHash
    }

    def allNeededRequiredSignersKeyHashes(
        transaction: Transaction
    ): Set[AddrKeyHash] = transaction.body.value.requiredSigners

    private def neededInputsKeyHashes[
        ExceptionT <: TransactionException.BadInputsUTxOException |
            TransactionException.BadCollateralInputsUTxOException
    ](
        transactionId: TransactionHash,
        inputs: Set[TransactionInput],
        utxo: UTxO,
        missingUTxOException: TransactionHash => ExceptionT
    ): Either[ExceptionT, Set[AddrKeyHash | StakeKeyHash]] = boundary {
        val result = for
            input <- inputs
            keyHash <- utxo.get(input) match
                case Some(output) => output.address.keyHash
                // This check allows to be an order independent in the sequence of validation rules
                case None => break(Left(missingUTxOException(transactionId)))
        yield keyHash
        Right(result)
    }
}
