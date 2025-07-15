package scalus.cardano.ledger
package utils

import scala.collection.View
import scala.util.boundary
import scala.util.boundary.break

object AllProvidedReferenceScripts {
    def allProvidedReferenceScripts(
        transaction: Transaction,
        utxo: UTxO
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Set[Script]
    ] = {
        allProvidedReferenceScriptsView(transaction, utxo).map(_.toSet)
    }

    def allProvidedReferenceScriptHashes(
        transaction: Transaction,
        utxo: UTxO
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Set[ScriptHash]
    ] = {
        allProvidedReferenceScriptHashesView(transaction, utxo).map(_.toSet)
    }

    def allProvidedReferenceScriptHashesView(
        transaction: Transaction,
        utxo: UTxO
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      View[ScriptHash]
    ] = {
        allProvidedReferenceScriptsView(transaction, utxo).map(_.map(_.scriptHash))
    }

    def allProvidedReferenceNativeScripts(
        transaction: Transaction,
        utxo: UTxO
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Set[Script.Native]
    ] = {
        allProvidedReferenceNativeScriptsView(transaction, utxo).map(_.toSet)
    }

    def allProvidedReferenceNativeScriptsView(
        transaction: Transaction,
        utxo: UTxO
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      View[Script.Native]
    ] = {
        allProvidedReferenceScriptsView(transaction, utxo).map(_.flatMap {
            case timelock: Script.Native => Some(timelock)
            case _                       => None
        })
    }

    def allProvidedReferenceNativeScriptHashes(
        transaction: Transaction,
        utxo: UTxO
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Set[ScriptHash]
    ] = {
        allProvidedReferenceNativeScriptHashesView(transaction, utxo).map(_.toSet)
    }

    def allProvidedReferenceNativeScriptHashesView(
        transaction: Transaction,
        utxo: UTxO
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      View[ScriptHash]
    ] = {
        allProvidedReferenceNativeScriptsView(transaction, utxo).map(_.map(_.scriptHash))
    }

    def allProvidedReferenceScriptsView(
        transaction: Transaction,
        utxo: UTxO
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      View[Script]
    ] = {
        for
            allProvidedInputsReferenceScripts <- allProvidedInputsReferenceScripts(
              transaction,
              utxo
            )
            allProvidedReferenceInputsReferenceScripts <-
                allProvidedReferenceInputsReferenceScripts(
                  transaction,
                  utxo
                )
        yield allProvidedInputsReferenceScripts.view ++
            allProvidedReferenceInputsReferenceScripts.view
    }

    def allProvidedInputsReferenceScripts(
        transaction: Transaction,
        utxo: UTxO
    ): Either[TransactionException.BadInputsUTxOException, Set[Script]] = {
        providedReferenceScripts(
          transaction.id,
          transaction.body.value.inputs,
          utxo,
          TransactionException.BadInputsUTxOException(_)
        )
    }

    def allProvidedInputsReferenceScriptHashes(
        transaction: Transaction,
        utxo: UTxO
    ): Either[TransactionException.BadInputsUTxOException, Set[ScriptHash]] = {
        allProvidedInputsReferenceScriptHashesView(transaction, utxo).map(_.toSet)
    }

    def allProvidedInputsReferenceScriptHashesView(
        transaction: Transaction,
        utxo: UTxO
    ): Either[TransactionException.BadInputsUTxOException, View[ScriptHash]] = {
        allProvidedInputsReferenceScripts(
          transaction,
          utxo
        ).map(_.view.map(_.scriptHash))
    }

    def allProvidedReferenceInputsReferenceScripts(
        transaction: Transaction,
        utxo: UTxO
    ): Either[TransactionException.BadReferenceInputsUTxOException, Set[Script]] = {
        providedReferenceScripts(
          transaction.id,
          transaction.body.value.referenceInputs,
          utxo,
          TransactionException.BadReferenceInputsUTxOException(_)
        )
    }

    def allProvidedReferenceInputsReferenceScriptHashes(
        transaction: Transaction,
        utxo: UTxO
    ): Either[TransactionException.BadReferenceInputsUTxOException, Set[ScriptHash]] = {
        allProvidedReferenceInputsReferenceScriptHashesView(transaction, utxo).map(_.toSet)
    }

    def allProvidedReferenceInputsReferenceScriptHashesView(
        transaction: Transaction,
        utxo: UTxO
    ): Either[TransactionException.BadReferenceInputsUTxOException, View[ScriptHash]] = {
        allProvidedReferenceInputsReferenceScripts(
          transaction,
          utxo
        ).map(_.view.map(_.scriptHash))
    }

    private def providedReferenceScripts[
        ExceptionT <: TransactionException.BadInputsUTxOException |
            TransactionException.BadReferenceInputsUTxOException
    ](
        transactionId: TransactionHash,
        inputs: Set[TransactionInput],
        utxo: UTxO,
        missingUTxOException: TransactionHash => ExceptionT
    ): Either[ExceptionT, Set[Script]] = boundary {
        val result = for
            input <- inputs
            script <- utxo.get(input) match
                case Some(output) => output.scriptRef.map(_.script)
                // This check allows to be an order independent in the sequence of validation rules
                case None => break(Left(missingUTxOException(transactionId)))
        yield script
        Right(result)
    }
}
