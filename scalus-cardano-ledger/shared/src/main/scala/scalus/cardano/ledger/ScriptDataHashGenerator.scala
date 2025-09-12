package scalus.cardano.ledger

import scalus.builtin.{platform, ByteString, Data, PlatformSpecific, given}
import scalus.ledger.babbage.ProtocolParams

import scala.collection.immutable
import scala.collection.immutable.{ListMap, TreeSet}

import scalus.cardano.ledger.utils.{AllNeededScriptHashes, AllResolvedScripts}

object ScriptDataHashGenerator {

    @deprecated
    def getUsedCostModels(
        pparams: ProtocolParams,
        w: TransactionWitnessSet,
        refLangs: TreeSet[Language]
    ): CostModels = {
        // FIXME:  reuse code from ledger rules
        val v1: TreeSet[Language] =
            if w.plutusV1Scripts.nonEmpty then TreeSet(Language.PlutusV1) else TreeSet.empty
        val v2: TreeSet[Language] =
            if w.plutusV2Scripts.nonEmpty then TreeSet(Language.PlutusV2) else TreeSet.empty
        val v3: TreeSet[Language] =
            if w.plutusV3Scripts.nonEmpty then TreeSet(Language.PlutusV3) else TreeSet.empty
        // must be sorted to ensure the same order of cost models
        // must preserve order of languages during traversal, so we use ListMap
        val models = (refLangs ++ v1 ++ v2 ++ v3).view
            .map(l => l.ordinal -> pparams.costModels(l.toString))
            .to(ListMap)
        CostModels(models)
    }

    def computeScriptDataHash(
        era: Era,
        redeemers: Option[KeepRaw[Redeemers]],
        datums: KeepRaw[TaggedSet[KeepRaw[Data]]],
        costModels: CostModels
    ): ScriptDataHash = {
        require(
          era.value >= Era.Conway.value,
          s"Script data hash generation is not supported for eras before Conway, got era: $era"
        )
        /* ; script data format:
         * ; [ redeemers | datums | language views ]
         * ; The redeemers are exactly the data present in the transaction witness set.
         * ; Similarly for the datums, if present. If no datums are provided, the middle
         * ; field is an empty string.
         */

        val redeemerBytes = redeemers match
            case Some(value) => value.raw
            case None        => Array(0xa0.toByte) // Empty map in CBOR
        val plutusDataBytes =
            if datums.value.toIndexedSeq.isEmpty then Array.empty[Byte]
            else datums.raw
        val costModelsBytes = costModels.getLanguageViewEncoding
        val encodedBytes = redeemerBytes ++ plutusDataBytes ++ costModelsBytes
        Hash(platform.blake2b_256(ByteString.unsafeFromArray(encodedBytes)))
    }

    def computeScriptDataHash(
        witnessSet: TransactionWitnessSet,
        era: Era,
        protocolParams: ProtocolParams,
        refLangs: TreeSet[Language],
        redeemers: Option[KeepRaw[Redeemers]],
        datums: KeepRaw[TaggedSet[KeepRaw[Data]]]
    ): Option[ScriptDataHash] = {
        val costModels = getUsedCostModels(protocolParams, witnessSet, refLangs)

        if redeemers.isEmpty && datums.value.toIndexedSeq.isEmpty && costModels.models.isEmpty then
            None
        else Some(computeScriptDataHash(era, redeemers, datums, costModels))
    }

    def computeScriptDataHash(
        transaction: Transaction,
        utxo: UTxO,
        era: Era,
        protocolParams: ProtocolParams
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Option[ScriptDataHash]
    ] = {
        val witnessSet = transaction.witnessSet
        val redeemers = witnessSet.redeemers
        val datums = witnessSet.plutusData

        for
            allNeededScriptHashes <- AllNeededScriptHashes.allNeededScriptHashes(transaction, utxo)

            allResolvedPlutusScripts <- AllResolvedScripts.allResolvedPlutusScriptsView(
              transaction,
              utxo
            )
        yield
            val languages = TreeSet.from(
              allResolvedPlutusScripts
                  .filter(plutusScript => allNeededScriptHashes.contains(plutusScript.scriptHash))
                  .map(_.language)
            )

            computeScriptDataHash(
              witnessSet,
              era,
              protocolParams,
              languages,
              redeemers,
              datums
            )
    }
}
