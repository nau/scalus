package scalus.cardano.ledger
package rules

import scala.collection.View
import scalus.cardano.ledger.utils.AllResolvedScripts

// It's Babbage.validateScriptsWellFormed in cardano-ledger
object ScriptsWellFormedValidator extends STS.Validator {
    override final type Error = TransactionException.IllFormedScriptsException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionId = event.id
        val utxo = state.utxo
        val body = event.body.value

        val allWitnessesPlutusScripts = AllResolvedScripts.allWitnessesPlutusScriptsView(event)

        val allOutputs = (body.outputs.view ++ body.collateralReturnOutput).map(_.value)
        val allPlutusScriptsFromAllOutputs =
            for
                output <- allOutputs
                scriptRef <- output.scriptRef
                plutusScript <- scriptRef.script match
                    case plutusScript: PlutusScript => Some(plutusScript)
                    case _                          => None
            yield plutusScript

        val allInvalidWitnessesPlutusScriptHashes = findAllInvalidPlutusScripts(
          allWitnessesPlutusScripts
        )

        val allInvalidPlutusScriptHashesFromAllOutputs = findAllInvalidPlutusScripts(
          allPlutusScriptsFromAllOutputs
        )

        if allInvalidWitnessesPlutusScriptHashes.nonEmpty ||
            allInvalidPlutusScriptHashesFromAllOutputs.nonEmpty
        then
            failure(
              TransactionException.IllFormedScriptsException(
                transactionId,
                allInvalidWitnessesPlutusScriptHashes,
                allInvalidPlutusScriptHashesFromAllOutputs
              )
            )
        else success
    }

    private def findAllInvalidPlutusScripts(plutusScripts: View[PlutusScript]): Set[ScriptHash] = {
        plutusScripts.filterNot(isWellFormedPlutusScript).map(_.scriptHash).toSet
    }

    private def isWellFormedPlutusScript(plutusScript: PlutusScript): Boolean = {
        ???
    }
}
