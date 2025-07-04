package scalus.cardano.ledger
package utils

import scala.util.boundary
import scala.util.boundary.break

object AllReferenceScripts {
    def allReferenceScripts(
        utxo: Utxo,
        transaction: Transaction
    ): Either[Throwable, Set[Script]] = allReferenceScripts(utxo, transaction, Some(_))

    def allReferenceScriptHashes(
        utxo: Utxo,
        transaction: Transaction
    ): Either[Throwable, Set[ScriptHash]] =
        allReferenceScripts(utxo, transaction, script => Some(script.scriptHash))

    def allReferenceNativeScripts(
        utxo: Utxo,
        transaction: Transaction
    ): Either[Throwable, Set[Script.Native]] = allReferenceScripts(
      utxo,
      transaction,
      {
          case timelock: Script.Native => Some(timelock)
          case _                       => None
      }
    )

    def allReferenceNativeScriptHashes(
        utxo: Utxo,
        transaction: Transaction
    ): Either[Throwable, Set[ScriptHash]] = allReferenceScripts(
      utxo,
      transaction,
      {
          case Script.Native(timelock) => Some(timelock.scriptHash)
          case _                       => None
      }
    )

    private def allReferenceScripts[T](
        utxo: Utxo,
        transaction: Transaction,
        mapper: Script => Option[T]
    ): Either[Throwable, Set[T]] = boundary {
        val result = (
          inputReferenceScriptsView(utxo, transaction) ++
              referenceInputReferenceScriptsView(utxo, transaction)
        ).flatMap {
            case Right(script) => mapper(script)
            case Left(error)   => break(Left(error))
        }

        Right(result.toSet)
    }

    private def inputReferenceScriptsView(
        utxo: Utxo,
        transaction: Transaction
    ): scala.collection.View[Either[Throwable, Script]] = {
        transactionReferenceScriptsView(
          transaction.body.value.inputs,
          transaction.id,
          utxo,
          (transactionId, input, index) =>
              IllegalArgumentException(
                s"Missing input $input with index $index in UTxO state for transactionId $transactionId"
              )
        )
    }

    private def referenceInputReferenceScriptsView(
        utxo: Utxo,
        transaction: Transaction
    ): scala.collection.View[Either[Throwable, Script]] = {
        transactionReferenceScriptsView(
          transaction.body.value.referenceInputs,
          transaction.id,
          utxo,
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
    ): scala.collection.View[Either[Throwable, Script]] = {
        for
            (input, index) <- inputs.view.zipWithIndex
            result <- utxo.get(input) match
                case Some(output) => output.scriptRef.map(scriptRef => Right(scriptRef.script))
                // This check allows to be an order independent in the sequence of validation rules
                case None => Some(Left(missingInputError(transactionId, input, index)))
        yield result
    }
}
