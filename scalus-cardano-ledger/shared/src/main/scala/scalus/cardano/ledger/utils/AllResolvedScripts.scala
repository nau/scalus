package scalus.cardano.ledger
package utils

import scala.collection.View

object AllResolvedScripts {
    export AllWitnessesScripts.*
    export AllProvidedReferenceScripts.*

    def allResolvedScriptsMap(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Map[ScriptHash, Script]
    ] = {
        allResolvedScriptsView(transaction, utxo).map(
          _.map(script => script.scriptHash -> script).toMap
        )
    }

    def allResolvedScripts(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Set[Script]
    ] = {
        allResolvedScriptsView(transaction, utxo).map(_.toSet)
    }

    def allResolvedScriptsView(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      View[Script]
    ] = {
        for
            allProvidedReferenceScriptsView <- allProvidedReferenceScriptsView(transaction, utxo)
            allWitnessesScriptsView = AllWitnessesScripts.allWitnessesScriptsView(transaction)
        yield allWitnessesScriptsView ++ allProvidedReferenceScriptsView
    }

    def allResolvedScriptHashes(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Set[ScriptHash]
    ] = {
        allResolvedScriptHashesView(transaction, utxo).map(_.toSet)
    }

    def allResolvedScriptHashesView(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      View[ScriptHash]
    ] = {
        allResolvedScriptsView(transaction, utxo).map(_.map(_.scriptHash))
    }

    def allResolvedPlutusScriptsMap(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Map[ScriptHash, PlutusScript]
    ] = {
        allResolvedPlutusScriptsView(transaction, utxo).map(
          _.map(script => script.scriptHash -> script).toMap
        )
    }

    def allResolvedPlutusScripts(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Set[PlutusScript]
    ] = {
        allResolvedPlutusScriptsView(transaction, utxo).map(_.toSet)
    }

    def allResolvedPlutusScriptsView(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      View[PlutusScript]
    ] = {
        for
            allProvidedReferencePlutusScriptsView <- allProvidedReferencePlutusScriptsView(
              transaction,
              utxo
            )
            allWitnessesPlutusScriptsView = AllWitnessesScripts.allWitnessesPlutusScriptsView(
              transaction
            )
        yield allWitnessesPlutusScriptsView ++ allProvidedReferencePlutusScriptsView
    }

    def allResolvedPlutusScriptHashes(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Set[ScriptHash]
    ] = {
        allResolvedPlutusScriptHashesView(transaction, utxo).map(_.toSet)
    }

    def allResolvedPlutusScriptHashesView(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      View[ScriptHash]
    ] = {
        allResolvedPlutusScriptsView(transaction, utxo).map(_.map(_.scriptHash))
    }

    def allResolvedNativeScriptsMap(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Map[ScriptHash, Script.Native]
    ] = {
        allResolvedNativeScriptsView(transaction, utxo).map(
          _.map(script => script.scriptHash -> script).toMap
        )
    }

    def allResolvedNativeScripts(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Set[Script.Native]
    ] = {
        allResolvedNativeScriptsView(transaction, utxo).map(_.toSet)
    }

    def allResolvedNativeScriptsView(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      View[Script.Native]
    ] = {
        for
            allProvidedReferenceNativeScriptsView <- allProvidedReferenceNativeScriptsView(
              transaction,
              utxo
            )
            allWitnessesNativeScriptsView = AllWitnessesScripts
                .allWitnessesNativeScripts(transaction)
                .view
        yield allWitnessesNativeScriptsView ++ allProvidedReferenceNativeScriptsView
    }

    def allResolvedNativeScriptHashes(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Set[ScriptHash]
    ] = {
        allResolvedNativeScriptHashesView(transaction, utxo).map(_.toSet)
    }

    def allResolvedNativeScriptHashesView(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      View[ScriptHash]
    ] = {
        allResolvedNativeScriptsView(transaction, utxo).map(_.map(_.scriptHash))
    }
}
