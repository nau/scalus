package scalus.cardano.ledger
package rules

import scalus.builtin.{ByteString, PlatformSpecific, given}

// It's Shelley.validateVerifiedWits in cardano-ledger
object VerifiedWitnessesValidator extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = {
        for {
            _ <- validateVkeyWitnesses(context, state, event)
            _ <- validateBootstrapWitnesses(context, state, event)
        } yield ()
    }

    private[this] def validateVkeyWitnesses(
        context: Context,
        state: State,
        event: Event
    ): Result =
        validate(
          event.id,
          event.witnessSet.vkeyWitnesses.view.map { vkeyWitness =>
              (vkeyWitness.vkey, vkeyWitness.signature)
          },
          (transactionId, key, signature, index) =>
              IllegalArgumentException(
                s"Invalid vkeyWitness at index $index for transactionId $transactionId: " +
                    s"key=$key, " +
                    s"signature=$signature"
              )
        )

    private[this] def validateBootstrapWitnesses(
        context: Context,
        state: State,
        event: Event
    ): Result =
        validate(
          event.id,
          event.witnessSet.bootstrapWitnesses.view.map { bootstrapWitness =>
              (bootstrapWitness.publicKey, bootstrapWitness.signature)
          },
          (transactionId, key, signature, index) =>
              IllegalArgumentException(
                s"Invalid bootstrapWitness at index $index for transactionId $transactionId: " +
                    s"key=$key, " +
                    s"signature=$signature"
              )
        )

    private[this] def validate(
        transactionId: TransactionHash,
        keysAndSignatures: scala.collection.View[(ByteString, ByteString)],
        error: (TransactionHash, ByteString, ByteString, Int) => IllegalArgumentException
    ): Result = {
        val result = keysAndSignatures.zipWithIndex.find { case ((key, signature), index) =>
            !summon[PlatformSpecific].verifyEd25519Signature(key, transactionId, signature)
        }

        result match
            case None => success
            case Some(((key, signature), index)) =>
                failure(error(transactionId, key, signature, index))

    }
}
