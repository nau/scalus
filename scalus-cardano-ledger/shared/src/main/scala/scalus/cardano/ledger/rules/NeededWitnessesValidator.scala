package scalus.cardano.ledger
package rules

import scalus.cardano.address.{Address, ShelleyPaymentPart, StakePayload}
import scala.util.boundary
import scala.util.boundary.break
import scala.util.control.NonFatal

// It's Shelley.validateNeededWitnesses in cardano-ledger
object NeededWitnessesValidator extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = {
        for
            _ <- validateInputs(context, state, event)
            _ <- validateCollateralInputs(context, state, event)
            _ <- validateVotingProcedures(context, state, event)
            _ <- validateCertificates(context, state, event)
        yield ()
    }

    private[this] def validateInputs(
        context: Context,
        state: State,
        event: Event
    ): Result =
        validateTransactionInputs(
          event.id,
          event.body.value.inputs,
          event.witnessSet.vkeyWitnesses,
          state.utxo,
          (transactionId, input, index) =>
              IllegalArgumentException(
                s"Missing input $input with index $index in UTxO state for transactionId $transactionId"
              ),
          (transactionId, input, index, exception) =>
              IllegalArgumentException(
                s"Invalid address format for input $input with index $index in UTxO state for transactionId $transactionId",
                exception
              ),
          (transactionId, hash, input, index) =>
              IllegalArgumentException(
                s"Missing vkey witness for staking credential $hash in input $input with index $index for transactionId $transactionId"
              )
        )

    private[this] def validateCollateralInputs(
        context: Context,
        state: State,
        event: Event
    ): Result =
        validateTransactionInputs(
          event.id,
          event.body.value.collateralInputs,
          event.witnessSet.vkeyWitnesses,
          state.utxo,
          (transactionId, collateralInput, index) =>
              IllegalArgumentException(
                s"Missing collateralInput $collateralInput with index $index in UTxO state for transactionId $transactionId"
              ),
          (transactionId, collateralInput, index, exception) =>
              IllegalArgumentException(
                s"Invalid address format for collateralInput $collateralInput with index $index in UTxO state for transactionId $transactionId",
                exception
              ),
          (transactionId, hash, collateralInput, index) =>
              IllegalArgumentException(
                s"Missing vkey witness for staking credential $hash in collateralInput $collateralInput with index $index for transactionId $transactionId"
              )
        )

    private[this] def validateVotingProcedures(
        context: Context,
        state: State,
        event: Event
    ): Result = boundary {
        val vkeyWitnesses = event.witnessSet.vkeyWitnesses

        val votingProcedures =
            event.body.value.votingProcedures.map(_.procedures).getOrElse(Map.empty)
        for (voter, index) <- votingProcedures.keySet.zipWithIndex
        do
            val OptionHash = voter match
                case Voter.ConstitutionalCommitteeHotKey(keyHash) => Some(keyHash)
                case Voter.StakingPoolKey(keyHash)                => Some(keyHash)
                case Voter.DRepKey(keyHash)                       => Some(keyHash)
                case _: Voter.ConstitutionalCommitteeHotScript    => None
                case _: Voter.DRepScript                          => None

            OptionHash.foreach { hash =>
                if !vkeyWitnesses.exists(_.vkeyHash == hash)
                then
                    break(
                      failure(
                        IllegalArgumentException(
                          s"Missing vkey witness for staking credential $hash in voter $voter with index $index for transactionId ${event.id}"
                        )
                      )
                    )
            }

        success
    }

    private[this] def validateCertificates(
        context: Context,
        state: State,
        event: Event
    ): Result = {
        ???
    }

    // TODO add bootstrap witnesses validation
    private[this] def validateTransactionInputs(
        transactionId: TransactionHash,
        inputs: Set[TransactionInput],
        vkeyWitnesses: Set[VKeyWitness],
        utxo: Utxo,
        missingInputError: (TransactionHash, TransactionInput, Int) => IllegalArgumentException,
        invalidAddressError: (
            TransactionHash,
            TransactionInput,
            Int,
            Throwable
        ) => IllegalArgumentException,
        missingWitnessError: (
            TransactionHash,
            Hash[Blake2b_224, HashPurpose.KeyHash | HashPurpose.StakeKeyHash],
            TransactionInput,
            Int
        ) => IllegalArgumentException
    ): Result = boundary {
        for (input, index) <- inputs.view.zipWithIndex
        do
            utxo.get(input) match
                case Some(output) =>
                    val address =
                        try Address.fromByteString(output.address)
                        catch
                            case NonFatal(exception) =>
                                break(
                                  failure(
                                    invalidAddressError(
                                      transactionId,
                                      input,
                                      index,
                                      exception
                                    )
                                  )
                                )

                    val optionHash = address match
                        case Address.Byron(_) =>
                            None // Byron addresses don't have staking credentials
                        case Address.Shelley(shelleyAddress) =>
                            shelleyAddress.payment match
                                case ShelleyPaymentPart.Key(hash) => Some(hash)
                                case _: ShelleyPaymentPart.Script => None
                        case Address.Stake(stakeAddress) =>
                            stakeAddress.payload match
                                case StakePayload.Stake(hash) => Some(hash)
                                case _: StakePayload.Script   => None

                    optionHash.foreach { hash =>
                        if !vkeyWitnesses.exists(_.vkeyHash == hash)
                        then
                            break(
                              failure(
                                missingWitnessError(
                                  transactionId,
                                  hash,
                                  input,
                                  index
                                )
                              )
                            )
                    }

                case None =>
                    break(failure(missingInputError(transactionId, input, index)))

        success
    }
}
