package scalus.cardano.ledger
package utils

import scala.collection.View

object AllWitnessesScripts {
    def allWitnessesScriptHashes(transaction: Transaction): Set[ScriptHash] =
        allWitnessesScriptHashesView(transaction).toSet

    def allWitnessesScriptHashesView(transaction: Transaction): View[ScriptHash] = {
        allWitnessesScriptsView(transaction).map(_.scriptHash)
    }

    def allWitnessesScripts(transaction: Transaction): Set[Script] = allWitnessesScriptsView(
      transaction
    ).toSet

    def allWitnessesScriptsView(transaction: Transaction): View[Script] = {
        val witnessSet = transaction.witnessSet

        witnessSet.nativeScripts.view ++
            witnessSet.plutusV1Scripts.view ++
            witnessSet.plutusV2Scripts.view ++
            witnessSet.plutusV3Scripts.view

    }

    def allWitnessesNativeScripts(transaction: Transaction): Set[Script.Native] =
        transaction.witnessSet.nativeScripts

    def allWitnessesNativeScriptHashes(transaction: Transaction): Set[ScriptHash] = {
        allWitnessesNativeScriptHashesView(transaction).toSet
    }

    def allWitnessesNativeScriptHashesView(transaction: Transaction): View[ScriptHash] = {
        allWitnessesNativeScripts(transaction).view.map(_.scriptHash)
    }
}
