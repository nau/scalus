package scalus.cardano.ledger

import scalus.builtin.ByteString

import scala.jdk.CollectionConverters.*

object BloxbeanToLedgerTranslation {
    extension (self: com.bloxbean.cardano.client.transaction.spec.Value)
        def toLedgerValue: Value = {
            Value(
              Coin(self.getCoin.longValue()),
              multiAsset = self.getMultiAssets.asScala.map { ma =>
                  Hash.scriptHash(ByteString.fromHex(ma.getPolicyId)) -> ma.getAssets.asScala.map {
                      asset =>
                          val name =
                              if asset.getName.startsWith("0x") then
                                  AssetName.fromHex(asset.getName.drop(2))
                              else AssetName.fromString(asset.getName)
                          name -> asset.getValue.longValue()
                  }.toMap
              }.toMap
            )
        }
}
