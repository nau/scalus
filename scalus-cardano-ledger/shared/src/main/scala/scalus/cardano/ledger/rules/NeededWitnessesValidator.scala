package scalus.cardano.ledger
package rules

import scala.util.boundary
import scala.util.boundary.break

// It's Shelley.validateNeededWitnesses in cardano-ledger
object NeededWitnessesValidator extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = {
        for
            _ <- validateInputs(context, state, event)
            _ <- validateCollateralInputs(context, state, event)
            _ <- validateVotingProcedures(context, state, event)
            _ <- validateWithdrawals(context, state, event)
            _ <- validateCertificates(context, state, event)
        yield ()
    }

    private def validateInputs(
        context: Context,
        state: State,
        event: Event
    ): Result =
        validateTransactionInputs(
          event.body.value.inputs,
          event.id,
          event.witnessSet.vkeyWitnesses,
          state.utxo,
          (transactionId, keyHash, input, index) =>
              IllegalArgumentException(
                s"Missing vkey witness for staking credential $keyHash in input $input with index $index for transactionId $transactionId"
              ),
          (transactionId, input, index) =>
              IllegalArgumentException(
                s"Missing input $input with index $index in UTxO state for transactionId $transactionId"
              )
        )

    private def validateCollateralInputs(
        context: Context,
        state: State,
        event: Event
    ): Result =
        validateTransactionInputs(
          event.body.value.collateralInputs,
          event.id,
          event.witnessSet.vkeyWitnesses,
          state.utxo,
          (transactionId, keyHash, collateralInput, index) =>
              IllegalArgumentException(
                s"Missing vkey witness for staking credential $keyHash in collateralInput $collateralInput with index $index for transactionId $transactionId"
              ),
          (transactionId, collateralInput, index) =>
              IllegalArgumentException(
                s"Missing collateralInput $collateralInput with index $index in UTxO state for transactionId $transactionId"
              )
        )

    private def validateVotingProcedures(
        context: Context,
        state: State,
        event: Event
    ): Result = boundary {
        val transactionId = event.id
        val vkeyWitnesses = event.witnessSet.vkeyWitnesses
        val votingProcedures =
            event.body.value.votingProcedures.map(_.procedures).getOrElse(Map.empty)

        for
            (voter, index) <- votingProcedures.view.keySet.zipWithIndex
            keyHash <- voter match
                case Voter.ConstitutionalCommitteeHotKey(keyHash) => Some(keyHash)
                case Voter.StakingPoolKey(keyHash)                => Some(keyHash)
                case Voter.DRepKey(keyHash)                       => Some(keyHash)
                case _: Voter.ConstitutionalCommitteeHotScript    => None
                case _: Voter.DRepScript                          => None
        do
            if !vkeyWitnesses.exists(_.vkeyHash == keyHash)
            then
                break(
                  failure(
                    IllegalArgumentException(
                      s"Missing vkey witness for staking credential $keyHash in voter $voter with index $index for transactionId $transactionId"
                    )
                  )
                )

        success
    }

    private def validateCertificates(
        context: Context,
        state: State,
        event: Event
    ): Result = boundary {
        val transactionId = event.id
        val vkeyWitnesses = event.witnessSet.vkeyWitnesses
        val certificates = event.body.value.certificates

        def extractKeyHash(credential: Credential): Option[AddrKeyHash] = {
            credential match
                case Credential.KeyHash(keyHash) => Some(keyHash)
                case _: Credential.ScriptHash    => None
        }

        for
            (certificate, index) <- certificates.view.zipWithIndex
            keyHash <- certificate match
                case Certificate.StakeRegistration(credential)   => extractKeyHash(credential)
                case Certificate.StakeDeregistration(credential) => extractKeyHash(credential)
                case Certificate.StakeDelegation(credential, _)  => extractKeyHash(credential)
                case certificate: Certificate.PoolRegistration   => Some(certificate.operator)
                case Certificate.PoolRetirement(poolKeyHash, _) =>
                    Some(poolKeyHash.asInstanceOf[AddrKeyHash])
                case Certificate.RegCert(credential, deposit) =>
                    if deposit > Coin.zero then extractKeyHash(credential)
                    else None // No witness needed for zero deposit
                case Certificate.UnregCert(credential, _)             => extractKeyHash(credential)
                case Certificate.VoteDelegCert(credential, _)         => extractKeyHash(credential)
                case Certificate.StakeVoteDelegCert(credential, _, _) => extractKeyHash(credential)
                case Certificate.StakeRegDelegCert(credential, _, _)  => extractKeyHash(credential)
                case Certificate.VoteRegDelegCert(credential, drep, coin) =>
                    extractKeyHash(credential)
                case Certificate.StakeVoteRegDelegCert(credential, _, _, _) =>
                    extractKeyHash(credential)
                case Certificate.AuthCommitteeHotCert(committeeColdCredential, _) =>
                    extractKeyHash(committeeColdCredential)
                case Certificate.ResignCommitteeColdCert(committeeColdCredential, _) =>
                    extractKeyHash(committeeColdCredential)
                case Certificate.RegDRepCert(drepCredential, _, _) => extractKeyHash(drepCredential)
                case Certificate.UnregDRepCert(drepCredential, _)  => extractKeyHash(drepCredential)
                case Certificate.UpdateDRepCert(drepCredential, _) => extractKeyHash(drepCredential)
        do
            if !vkeyWitnesses.exists(_.vkeyHash == keyHash) then
                break(
                  failure(
                    IllegalArgumentException(
                      s"Missing vkey witness for staking credential $keyHash in certificate $certificate with index $index for transactionId $transactionId"
                    )
                  )
                )

        success
    }

    private def validateWithdrawals(
        context: Context,
        state: State,
        event: Event
    ): Result = boundary {
        val transactionId = event.id
        val vkeyWitnesses = event.witnessSet.vkeyWitnesses
        val withdrawals = event.body.value.withdrawals.map(_.withdrawals).getOrElse(Map.empty)

        for
            (rewardAccount, index) <- withdrawals.view.keySet.zipWithIndex
            keyHash <- rewardAccount.address.keyHash
        do
            if !vkeyWitnesses.exists(_.vkeyHash == keyHash)
            then
                break(
                  failure(
                    IllegalArgumentException(
                      s"Missing vkey witness for staking credential $keyHash in reward account $rewardAccount with index $index in withdrawals for transactionId $transactionId"
                    )
                  )
                )

        success
    }

    // TODO add bootstrap witnesses validation
    private def validateTransactionInputs(
        inputs: Set[TransactionInput],
        transactionId: TransactionHash,
        vkeyWitnesses: Set[VKeyWitness],
        utxo: Utxo,
        missingWitnessError: (
            TransactionHash,
            Hash[Blake2b_224, HashPurpose.KeyHash | HashPurpose.StakeKeyHash],
            TransactionInput,
            Int
        ) => IllegalArgumentException,
        missingInputError: (TransactionHash, TransactionInput, Int) => IllegalArgumentException
    ): Result = boundary {
        for
            (input, index) <- inputs.view.zipWithIndex
            keyHash <- utxo.get(input) match
                case Some(output) => output.address.keyHash
                // This check allows to be an order independent in the sequence of validation rules
                case None => break(failure(missingInputError(transactionId, input, index)))
        do
            if !vkeyWitnesses.exists(_.vkeyHash == keyHash)
            then break(failure(missingWitnessError(transactionId, keyHash, input, index)))

        success
    }
}
