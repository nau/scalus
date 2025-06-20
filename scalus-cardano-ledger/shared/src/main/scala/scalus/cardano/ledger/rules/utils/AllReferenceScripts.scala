package scalus.cardano.ledger
package rules
package utils

import scala.util.boundary
import scala.util.boundary.break

trait AllReferenceScripts {
    this: STS.Validator =>

    protected def allReferenceScripts(
        state: State,
        event: Event
    ): Either[Error, Set[Script]] = boundary {
        allReferenceScriptsView(state, event).map(_.toSet)
    }

    protected def allReferenceScriptHashes(
        state: State,
        event: Event
    ): Either[Error, Set[ScriptHash]] = {
        allReferenceScriptsView(state, event).map(scripts => scripts.map(_.scriptHash).toSet)
    }

    private def allReferenceScriptsView(
        state: State,
        event: Event
    ): Either[Error, scala.collection.View[Script]] = boundary {
        val result = (
          inputReferenceScriptsView(state, event) ++ referenceInputReferenceScriptsView(
            state,
            event
          )
        ).map {
            case Right(script) => script
            case Left(error)   => break(Left(error))
        }

        Right(result)
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
                case Some(output) =>
                    output match
                        case babbage: TransactionOutput.Babbage =>
                            babbage.scriptRef match
                                case Some(ScriptRef(script)) => Some(Right(script))
                                case None                    => None
                        case _: TransactionOutput.Shelley => None
                // This check allows to be an order independent in the sequence of validation rules
                case None =>
                    Some(Left(missingInputError(transactionId, input, index)))
        yield result
    }
}
