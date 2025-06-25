package scalus.cardano.ledger
package rules
package utils

import scalus.ledger.api.Timelock

trait AllProvidedScripts {
    this: STS =>

    protected def allProvidedScriptHashes(event: Event): Set[ScriptHash] =
        allProvidedScriptHashesView(event).toSet

    protected def allProvidedScriptHashesView(event: Event): scala.collection.View[ScriptHash] = {
        val witnessSet = event.witnessSet

        witnessSet.nativeScripts.view.map(_.scriptHash) ++
            witnessSet.plutusV1Scripts.view.map(_.scriptHash) ++
            witnessSet.plutusV2Scripts.view.map(_.scriptHash) ++
            witnessSet.plutusV3Scripts.view.map(_.scriptHash)

    }

    protected def allProvidedNativeScripts(event: Event): Set[Timelock] =
        event.witnessSet.nativeScripts
}
