package scalus.cardano.ledger.rules

import scalus.builtin.{PlatformSpecific, given}

// It's Shelley.validateVerifiedWits in cardano-ledge
object VerifiedWitnessesValidator extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = {
        val vkeyWitnesses = event.witnessSet.vkeyWitnesses.getOrElse(Set.empty)

        val invalidSignatures = vkeyWitnesses.view.zipWithIndex
            .filter { case (vkeyWitness, index) =>
                !summon[PlatformSpecific].verifyEd25519Signature(
                  vkeyWitness.vkey,
                  event.id,
                  vkeyWitness.signature
                )
            }
            .map(_._2)
            .toSeq

        if invalidSignatures.isEmpty then success
        else
            failure(
              IllegalArgumentException(
                s"Invalid signatures for vkeyWitnesses with indexes: $invalidSignatures"
              )
            )
    }
}
