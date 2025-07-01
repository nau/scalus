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
            _ <- validateRequiredSigners(context, state, event)
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
                case cert: Certificate.StakeRegistration   => extractKeyHash(cert.credential)
                case cert: Certificate.StakeDeregistration => extractKeyHash(cert.credential)
                case cert: Certificate.StakeDelegation     => extractKeyHash(cert.credential)
                case cert: Certificate.PoolRegistration =>
                    cert.poolOwners.view.concat(Some(cert.operator))
                case cert: Certificate.PoolRetirement => Some(cert.poolKeyHash)
                case cert: Certificate.RegCert =>
                    if cert.coin > Coin.zero then extractKeyHash(cert.credential)
                    else None // No witness needed for zero deposit
                case cert: Certificate.UnregCert             => extractKeyHash(cert.credential)
                case cert: Certificate.VoteDelegCert         => extractKeyHash(cert.credential)
                case cert: Certificate.StakeVoteDelegCert    => extractKeyHash(cert.credential)
                case cert: Certificate.StakeRegDelegCert     => extractKeyHash(cert.credential)
                case cert: Certificate.VoteRegDelegCert      => extractKeyHash(cert.credential)
                case cert: Certificate.StakeVoteRegDelegCert => extractKeyHash(cert.credential)
                case cert: Certificate.AuthCommitteeHotCert =>
                    extractKeyHash(cert.committeeColdCredential)
                case cert: Certificate.ResignCommitteeColdCert =>
                    extractKeyHash(cert.committeeColdCredential)
                case cert: Certificate.RegDRepCert    => extractKeyHash(cert.drepCredential)
                case cert: Certificate.UnregDRepCert  => extractKeyHash(cert.drepCredential)
                case cert: Certificate.UpdateDRepCert => extractKeyHash(cert.drepCredential)
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

    private def validateRequiredSigners(context: Context, state: State, event: Event): Result =
        boundary {
            val transactionId = event.id
            val vkeyWitnesses = event.witnessSet.vkeyWitnesses
            val requiredSigners = event.body.value.requiredSigners

            for (keyHash, index) <- requiredSigners.view.zipWithIndex
            do
                if !vkeyWitnesses.exists(_.vkeyHash == keyHash)
                then
                    break(
                      failure(
                        IllegalArgumentException(
                          s"Missing vkey witness for required signer $keyHash with index $index for transactionId $transactionId"
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
