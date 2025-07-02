package scalus.cardano.ledger

import com.bloxbean.cardano.client.api.model.Utxo
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.Address
import scalus.utils.Hex

import scala.jdk.CollectionConverters.*

object BloxbeanToLedgerTranslation {
    extension (self: com.bloxbean.cardano.client.transaction.spec.Value)
        def toLedgerValue: Value = {
            Value(
              Coin(self.getCoin.longValue()),
              multiAsset = self.getMultiAssets.asScala.map { ma =>
                  Hash.scriptHash(ByteString.fromHex(ma.getPolicyId)) -> ma.getAssets.asScala.map {
                      asset =>
                          AssetName.fromString(asset.getName) -> asset.getValue.longValue()
                  }.toMap
              }.toMap
            )
        }

    def getTransactionOutput(output: Utxo): TransactionOutput = {
        val address = Address.fromBech32(output.getAddress)
        val datumOption: Option[DatumOption] =
            Option(output.getDataHash) -> Option(output.getInlineDatum) match
                case (Some(dataHash), None) =>
                    Some(DatumOption.Hash(Hash(ByteString.fromHex(dataHash))))
                case (None, Some(inlineDatum)) =>
                    Some(DatumOption.Inline(Data.fromCbor(Hex.hexToBytes(inlineDatum))))
                case (None, None) => None
                case (Some(_), Some(_)) =>
                    throw new IllegalStateException(
                      "Output cannot have both datum hash and inline datum"
                    )
        TransactionOutput.Babbage(
          address,
          output.toValue.toLedgerValue,
          datumOption,
          None
        ) // FIXME: scriptRefOpt is not handled here
    }

}
