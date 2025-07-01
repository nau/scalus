package scalus.cardano.ledger
package rules
package utils

import scala.collection.View

object AllProvidedScripts extends AllProvidedScripts
trait AllProvidedScripts {

    protected def allProvidedScriptHashes(tx: Transaction): Set[ScriptHash] =
        allProvidedScriptHashesView(tx).toSet

    protected def allProvidedScriptHashesView(tx: Transaction): View[ScriptHash] = {
        allProvidedScriptsView(tx).map(_.scriptHash)
    }

    def allProvidedScriptsView(tx: Transaction): View[Script] = {
        val witnessSet = tx.witnessSet

        witnessSet.nativeScripts.view ++
            witnessSet.plutusV1Scripts.view ++
            witnessSet.plutusV2Scripts.view ++
            witnessSet.plutusV3Scripts.view

    }

    protected def allProvidedNativeScripts(tx: Transaction): Set[Script.Native] =
        tx.witnessSet.nativeScripts
}
