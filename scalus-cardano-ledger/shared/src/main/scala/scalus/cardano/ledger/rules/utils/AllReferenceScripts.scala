package scalus.cardano.ledger
package rules
package utils

import scalus.ledger.api.Timelock
import scala.util.boundary
import scala.util.boundary.break

trait AllReferenceScripts {
    this: STS =>

    protected def allReferenceScripts(
        state: State,
        event: Event
    ): Either[Error, Set[Script]] = allReferenceScripts(state, event, Some(_))

    protected def allReferenceNativeScripts(
        state: State,
        event: Event
    ): Either[Error, Set[Timelock]] = allReferenceScripts(
      state,
      event,
      {
          case Script.Native(timelock) => Some(timelock)
          case _                       => None
      }
    )

    protected def allReferenceScriptHashes(
        state: State,
        event: Event
    ): Either[Error, Set[ScriptHash]] =
        allReferenceScripts(state, event, script => Some(script.scriptHash))

    private def allReferenceScripts[T](
        state: State,
        event: Event,
        mapper: Script => Option[T]
    ): Either[Error, Set[T]] = boundary {
        val result = (
          inputReferenceScriptsView(state, event) ++
              referenceInputReferenceScriptsView(state, event)
        ).flatMap {
            case Right(script) => mapper(script)
            case Left(error)   => break(Left(error))
        }

        Right(result.toSet)
    }

    private def inputReferenceScriptsView(
        state: State,
        event: Event
    ): scala.collection.View[Either[Error, Script]] = {
        transactionReferenceScriptsView(
          event.body.value.inputs,
          event.id,
          state.utxo,
          (transactionId, input, index) =>
              IllegalArgumentException(
                s"Missing input $input with index $index in UTxO state for transactionId $transactionId"
              )
        )
    }

    private def referenceInputReferenceScriptsView(
        state: State,
        event: Event
    ): scala.collection.View[Either[Error, Script]] = {
        transactionReferenceScriptsView(
          event.body.value.referenceInputs,
          event.id,
          state.utxo,
          (transactionId, input, index) =>
              IllegalArgumentException(
                s"Missing reference input $input with index $index in UTxO state for transactionId $transactionId"
              )
        )
    }

    private def transactionReferenceScriptsView(
        inputs: Set[TransactionInput],
        transactionId: TransactionHash,
        utxo: Utxo,
        missingInputError: (TransactionHash, TransactionInput, Int) => IllegalArgumentException
    ): scala.collection.View[Either[Error, Script]] = {
        for
            (input, index) <- inputs.view.zipWithIndex
            result <- utxo.get(input) match
                case Some(output) => output.scriptRef.map(scriptRef => Right(scriptRef.script))
                // This check allows to be an order independent in the sequence of validation rules
                case None => Some(Left(missingInputError(transactionId, input, index)))
        yield result
    }
}
