package scalus.cardano.ledger
package rules
package utils

trait AllProvidedScriptHashes {
    this: STS.Validator =>

    protected def allProvidedScriptHashes(event: Event): Set[ScriptHash] = {
        val witnessSet = event.witnessSet

        (
          witnessSet.nativeScripts.view.map(_.scriptHash) ++
              witnessSet.plutusV1Scripts.view.map(_.scriptHash) ++
              witnessSet.plutusV2Scripts.view.map(_.scriptHash) ++
              witnessSet.plutusV3Scripts.view.map(_.scriptHash)
        ).toSet
    }
}
