package scalus.cardano.ledger
package utils

import scala.collection.View

object AllWitnessScripts {
    def allWitnessScriptHashes(tx: Transaction): Set[ScriptHash] =
        allWitnessScriptHashesView(tx).toSet

    def allWitnessScriptHashesView(tx: Transaction): View[ScriptHash] = {
        allWitnessScriptsView(tx).map(_.scriptHash)
    }

    def allWitnessScripts(tx: Transaction): Set[Script] = allWitnessScriptsView(tx).toSet

    def allWitnessScriptsView(tx: Transaction): View[Script] = {
        val witnessSet = tx.witnessSet

        witnessSet.nativeScripts.view ++
            witnessSet.plutusV1Scripts.view ++
            witnessSet.plutusV2Scripts.view ++
            witnessSet.plutusV3Scripts.view

    }

    def allWitnessNativeScripts(tx: Transaction): Set[Script.Native] =
        tx.witnessSet.nativeScripts
}
