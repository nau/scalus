package scalus.cardano.ledger

import scalus.builtin.{ByteString, Data, PlatformSpecific, given}
import scalus.ledger.babbage.ProtocolParams

import scala.collection.immutable
import scala.collection.immutable.ListMap

object ScriptDataHashGenerator {

    def getCostModelsForTxWitness(pparams: ProtocolParams, w: TransactionWitnessSet): CostModels = {

        val cmv1 =
            if w.plutusV1Scripts.nonEmpty then
                ListMap(0 -> pparams.costModels("PlutusV1").toIndexedSeq)
            else ListMap.empty[Int, IndexedSeq[Long]]

        val cmv2 =
            if w.plutusV2Scripts.nonEmpty then
                ListMap(1 -> pparams.costModels("PlutusV2").toIndexedSeq)
            else ListMap.empty[Int, IndexedSeq[Long]]

        val cmv3 =
            if w.plutusV3Scripts.nonEmpty then
                ListMap(2 -> pparams.costModels("PlutusV3").toIndexedSeq)
            else ListMap.empty[Int, IndexedSeq[Long]]

        // FIXME: here we need to check script languages from reference inputs

        CostModels(cmv1 ++ cmv2 ++ cmv3)
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
        Hash(summon[PlatformSpecific].blake2b_256(ByteString.unsafeFromArray(encodedBytes)))
    }
}
