package scalus.cardano.ledger

import io.bullet.borer.Cbor
import scalus.builtin.{ByteString, Data, PlatformSpecific, given}

import java.util
import java.util.List
import scala.collection.immutable

object ScriptDataHashGenerator {
    def generate(
        era: Era,
        redeemers: Seq[Redeemer],
        datums: Seq[Data],
        costModels: CostModels
    ): Array[Byte] = {
        if era.value < Era.Conway.value then
            throw new IllegalArgumentException(
              s"Script data hash generation is not supported for eras before Conway, got era: $era"
            )
        /* ; script data format:
         * ; [ redeemers | datums | language views ]
         * ; The redeemers are exactly the data present in the transaction witness set.
         * ; Similarly for the datums, if present. If no datums are provided, the middle
         * ; field is an empty string.
         */

        val plutusDataBytes = Cbor.encode(datums.iterator.map(_.toCbor).toArray).toByteArray
        val redeemerBytes =
            if redeemers.isEmpty then Array(0xa0.toByte) // Empty map in CBOR
            else Cbor.encode(Redeemers.from(redeemers)).toByteArray
        val costModelsBytes = costModels.getLanguageViewEncoding
        val encodedBytes = redeemerBytes ++ plutusDataBytes ++ costModelsBytes
        summon[PlatformSpecific].blake2b_256(ByteString.unsafeFromArray(encodedBytes)).bytes
    }
}
