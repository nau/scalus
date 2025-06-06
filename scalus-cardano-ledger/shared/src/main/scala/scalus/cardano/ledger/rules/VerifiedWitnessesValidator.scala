package scalus.cardano.ledger
package rules

import scalus.builtin.{ByteString, PlatformSpecific, given}

// It's Shelley.validateVerifiedWits in cardano-ledge
object VerifiedWitnessesValidator extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = {
        val vkeyWitnesses = event.witnessSet.vkeyWitnesses.getOrElse(Set.empty)
        findFirstInvalidWitnessWithIndex(
          vkeyWitnesses.view.map { vkeyWitness =>
              (vkeyWitness.vkey, vkeyWitness.signature)
          },
          event.id
        ) match
            case None =>
            case Some((key, signature, index)) =>
                return failure(
                  IllegalArgumentException(
                    s"Invalid vkeyWitness at index $index for transactionId ${event.id}: " +
                        s"key=$key, " +
                        s"signature=$signature"
                  )
                )

        val bootstrapWitnesses = event.witnessSet.bootstrapWitnesses.getOrElse(Set.empty)
        findFirstInvalidWitnessWithIndex(
          bootstrapWitnesses.view.map { bootstrapWitness =>
              (bootstrapWitness.publicKey, bootstrapWitness.signature)
          },
          event.id
        ) match
            case None =>
            case Some((key, signature, index)) =>
                return failure(
                  IllegalArgumentException(
                    s"Invalid bootstrapWitness at index $index for transactionId ${event.id}: " +
                        s"key=$key, " +
                        s"signature=$signature"
                  )
                )

        success
    }

    private[this] def findFirstInvalidWitnessWithIndex(
        keysAndSignatures: scala.collection.View[(ByteString, ByteString)],
        transactionId: TransactionHash
    ): Option[(ByteString, ByteString, Int)] = {
        keysAndSignatures.zipWithIndex
            .map { case ((key, signature), index) =>
                (key, signature, index)
            }
            .find { case (key, signature, index) =>
                !summon[PlatformSpecific].verifyEd25519Signature(key, transactionId, signature)
            }
    }
}
