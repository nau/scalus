package scalus.cardano.ledger

import scalus.builtin.{platform, ByteString, Data, PlatformSpecific, given}
import scalus.ledger.babbage.ProtocolParams

import scala.collection.immutable
import scala.collection.immutable.{ListMap, TreeSet}

object ScriptDataHashGenerator {

    def getUsedCostModels(
        pparams: ProtocolParams,
        w: TransactionWitnessSet,
        refScripts: Set[Script]
    ): CostModels = {

        val v1 = if w.plutusV1Scripts.nonEmpty then Set(Language.PlutusV1) else Set.empty
        val v2 = if w.plutusV2Scripts.nonEmpty then Set(Language.PlutusV2) else Set.empty
        val v3 = if w.plutusV3Scripts.nonEmpty then Set(Language.PlutusV3) else Set.empty

        val refLangs: TreeSet[Language] = refScripts.view.flatMap(_.language).to(TreeSet)
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
        datums: KeepRaw[TaggedSet[Data]],
        costModels: CostModels
    ): DataHash = {
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
            if datums.value.isEmpty then Array.empty[Byte]
            else datums.raw
        val costModelsBytes = costModels.getLanguageViewEncoding
        val encodedBytes = redeemerBytes ++ plutusDataBytes ++ costModelsBytes
        Hash(platform.blake2b_256(ByteString.unsafeFromArray(encodedBytes)))
    }
}
