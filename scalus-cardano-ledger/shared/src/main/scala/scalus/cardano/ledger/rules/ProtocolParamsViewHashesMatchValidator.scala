package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.utils.{AllNeededScriptHashes, AllResolvedScripts}

import scala.collection.immutable.TreeSet

// It's ppViewHashesMatch in cardano-ledger
object ProtocolParamsViewHashesMatchValidator extends STS.Validator {
    override final type Error = TransactionException.BadInputsUTxOException |
        TransactionException.BadReferenceInputsUTxOException |
        TransactionException.InvalidScriptDataHashException

    override def validate(context: Context, state: State, event: Event): Result = {
        val utxo = state.utxo
        val expectedScriptDataHash = event.body.value.scriptDataHash
        val witnessSet = event.witnessSet
        val redeemers = witnessSet.redeemers
        val datums = witnessSet.plutusData
        val protocolParams = context.env.params

        for
            allNeededScriptHashes <- AllNeededScriptHashes.allNeededScriptHashes(event, utxo)

            allResolvedPlutusScripts <- AllResolvedScripts.allResolvedPlutusScriptsView(event, utxo)

            languages = TreeSet.from(
              allResolvedPlutusScripts
                  .filter(plutusScript => allNeededScriptHashes.contains(plutusScript.scriptHash))
                  .map(_.language)
            )

            actualScriptDataHash = ScriptDataHashGenerator.computeScriptDataHash(
              witnessSet,
              Era.Conway,
              protocolParams,
              languages,
              redeemers,
              datums
            )

            _ <-
                if actualScriptDataHash == expectedScriptDataHash then success
                else
                    failure(
                      TransactionException.InvalidScriptDataHashException(
                        event.id,
                        actualScriptDataHash,
                        expectedScriptDataHash
                      )
                    )
        yield ()
    }
}
