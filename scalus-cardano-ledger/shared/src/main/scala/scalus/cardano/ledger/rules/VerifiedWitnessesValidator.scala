package scalus.cardano.ledger
package rules

import scala.util.boundary
import scala.util.boundary.break
import scalus.builtin.{platform, ByteString, PlatformSpecific, given}

import scala.util.control.NonFatal

// It's Shelley.validateVerifiedWits in cardano-ledger
object VerifiedWitnessesValidator extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = {
        for {
            _ <- validateVkeyWitnesses(context, state, event)
            _ <- validateBootstrapWitnesses(context, state, event)
        } yield ()
    }

    private def validateVkeyWitnesses(
        context: Context,
        state: State,
        event: Event
    ): Result =
        validateWitnesses(
          event.id,
          event.witnessSet.vkeyWitnesses.view.map { vkeyWitness =>
              (vkeyWitness.vkey, vkeyWitness.signature)
          },
          (transactionId, key, signature, index, optionException) =>
              val message =
                  s"Invalid vkeyWitness at index $index for transactionId $transactionId: " +
                      s"key=$key, " +
                      s"signature=$signature"
              optionException match
                  case None            => IllegalArgumentException(message)
                  case Some(exception) => IllegalArgumentException(message, exception)
        )

    private def validateBootstrapWitnesses(
        context: Context,
        state: State,
        event: Event
    ): Result =
        validateWitnesses(
          event.id,
          event.witnessSet.bootstrapWitnesses.view.map { bootstrapWitness =>
              (bootstrapWitness.publicKey, bootstrapWitness.signature)
          },
          (transactionId, key, signature, index, optionException) =>
              val message =
                  s"Invalid bootstrapWitness at index $index for transactionId $transactionId: " +
                      s"key=$key, " +
                      s"signature=$signature"
              optionException match
                  case None            => IllegalArgumentException(message)
                  case Some(exception) => IllegalArgumentException(message, exception)
        )

    private def validateWitnesses(
        transactionId: TransactionHash,
        keysAndSignatures: scala.collection.View[(ByteString, ByteString)],
        invalidSignatureError: (
            TransactionHash,
            ByteString,
            ByteString,
            Int,
            Option[Throwable]
        ) => IllegalArgumentException,
    ): Result = boundary {
        for ((key, signature), index) <- keysAndSignatures.zipWithIndex
        do
            try
                if !platform.verifyEd25519Signature(key, transactionId, signature)
                then
                    break(
                      failure(invalidSignatureError(transactionId, key, signature, index, None))
                    )
            catch
                case NonFatal(exception) =>
                    break(
                      failure(
                        invalidSignatureError(transactionId, key, signature, index, Some(exception))
                      )
                    )

        success
    }
}
