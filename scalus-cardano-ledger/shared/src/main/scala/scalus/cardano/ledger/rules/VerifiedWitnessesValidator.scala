package scalus.cardano.ledger
package rules

import scalus.builtin.{platform, ByteString}
import scala.util.control.NonFatal

// It's Shelley.validateVerifiedWits in cardano-ledger
object VerifiedWitnessesValidator extends STS.Validator {
    override final type Error = TransactionException.InvalidVerifiedWitnessesException | Throwable

    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionId = event.id
        val utxo = state.utxo

        val invalidVkeyWitnessesSet = invalidVkeyWitnesses(event, utxo)
        val invalidBootstrapWitnessesSet = invalidBootstrapWitnesses(event, utxo)

        if invalidVkeyWitnessesSet.nonEmpty || invalidBootstrapWitnessesSet.nonEmpty
        then
            return failure(
              TransactionException.InvalidVerifiedWitnessesException(
                transactionId,
                invalidVkeyWitnessesSet,
                invalidBootstrapWitnessesSet
              )
            )

        success
    }

    private def invalidVkeyWitnesses(
        event: Event,
        utxo: UTxO
    ): Set[VKeyWitness] = {
        val transactionId = event.id
        val vkeyWitnesses = event.witnessSet.vkeyWitnesses

        vkeyWitnesses.filterNot(vkeyWitness =>
            verifyWitness(transactionId, vkeyWitness.vkey, vkeyWitness.signature)
        )
    }

    private def invalidBootstrapWitnesses(
        event: Event,
        utxo: UTxO
    ): Set[BootstrapWitness] = {
        val transactionId = event.id
        val bootstrapWitnesses = event.witnessSet.bootstrapWitnesses

        bootstrapWitnesses.filterNot(bootstrapWitness =>
            verifyWitness(transactionId, bootstrapWitness.publicKey, bootstrapWitness.signature)
        )
    }

    private def verifyWitness(
        transactionId: TransactionHash,
        key: ByteString,
        signature: ByteString
    ): Boolean = {
        try platform.verifyEd25519Signature(key, transactionId, signature)
        catch case NonFatal(exception) => false
    }
}
