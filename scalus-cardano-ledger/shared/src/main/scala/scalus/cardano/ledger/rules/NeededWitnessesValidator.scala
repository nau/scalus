package scalus.cardano.ledger
package rules

import scalus.cardano.address.Address
import scalus.ledger.api.v3.{Credential, PubKeyHash, StakingCredential}
import scala.util.boundary
import scala.util.boundary.break
import scala.util.control.NonFatal

// It's Shelley.validateNeededWitnesses in cardano-ledger
object NeededWitnessesValidator extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = {
        for
            _ <- validateInputs(context, state, event)
            _ <- validateCollateralInputs(context, state, event)
        yield ()
    }

    private[this] def validateInputs(
        context: Context,
        state: State,
        event: Event
    ): Result = boundary {
        val vkeyWitnesses = event.witnessSet.vkeyWitnesses

        for (input, index) <- event.body.value.inputs.view.zipWithIndex
        do
            state.utxo.get(input) match
                case Some(output) =>
                    val address =
                        try Address.fromByteString(output.address)
                        catch
                            case NonFatal(exception) =>
                                break(
                                  failure(
                                    IllegalArgumentException(
                                      s"Invalid address format for input $input with index $index in UTxO state for transactionId ${event.id}",
                                      exception
                                    )
                                  )
                                )
                    address.stakingCredential match {
                        case Some(
                              StakingCredential.StakingHash(
                                Credential.PubKeyCredential(PubKeyHash(hash))
                              )
                            ) =>
                            vkeyWitnesses.find { _.vkeyHash == hash } match
                                case Some(_) =>
                                case None =>
                                    break(
                                      failure(
                                        IllegalArgumentException(
                                          s"Missing vkey witness for staking credential $hash in input $input with index $index for transactionId ${event.id}"
                                        )
                                      )
                                    )
                        case _ =>
                    }
                case None =>
                    break(
                      failure(
                        IllegalArgumentException(
                          s"Missing input $input with index $index in UTxO state for transactionId ${event.id}"
                        )
                      )
                    )

        success
    }

    private[this] def validateCollateralInputs(
        context: Context,
        state: State,
        event: Event
    ): Result = boundary {
        val vkeyWitnesses = event.witnessSet.vkeyWitnesses

        for (collateralInput, index) <- event.body.value.collateralInputs.view.zipWithIndex
        do
            state.utxo.get(collateralInput) match
                case Some(output) =>
                    val address =
                        try Address.fromByteString(output.address)
                        catch
                            case NonFatal(exception) =>
                                break(
                                  failure(
                                    IllegalArgumentException(
                                      s"Invalid address format for collateralInput $collateralInput with index $index in UTxO state for transactionId ${event.id}",
                                      exception
                                    )
                                  )
                                )
                    address.stakingCredential match {
                        case Some(
                              StakingCredential.StakingHash(
                                Credential.PubKeyCredential(PubKeyHash(hash))
                              )
                            ) =>
                            vkeyWitnesses.find { _.vkeyHash == hash } match
                                case Some(_) =>
                                case None =>
                                    break(
                                      failure(
                                        IllegalArgumentException(
                                          s"Missing vkey witness for staking credential $hash in collateralInput $collateralInput with index $index for transactionId ${event.id}"
                                        )
                                      )
                                    )
                        case _ =>
                    }
                case None =>
                    break(
                      failure(
                        IllegalArgumentException(
                          s"Missing collateralInput $collateralInput with index $index in UTxO state for transactionId ${event.id}"
                        )
                      )
                    )

        success
    }
}
