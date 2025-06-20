package scalus.cardano.ledger

import io.bullet.borer.{Decoder, Encoder, Reader, Writer}
import scalus.cardano.ledger

/** Represents a value in Cardano, which can be either pure ADA or ADA with multi-assets */
case class Value(coin: Coin, assets: ledger.MultiAsset) {
    // Validate multi-asset map
    validateMultiAsset(assets)

    def binOp(op: (Long, Long) => Long)(rhs: Value): Value = (this, rhs) match
        case (Value(coin1, assets1), Value(coin2, assets2)) =>
            Value(Coin(op(coin1.value, coin2.value)), ledger.MultiAsset.binOp(op)(assets1, assets2))

    def +(rhs: Value): Value = binOp(_ + _)(rhs)

    def -(rhs: Value): Value = binOp(_ - _)(rhs)

    /** Validate a multi-asset map according to Cardano rules */
    private def validateMultiAsset(multiAsset: ledger.MultiAsset): Unit = {
        // Validate that all policy maps are non-empty
        require(
          multiAsset.forall { case (_, assets) => assets.nonEmpty },
          "MultiAsset map cannot contain empty policy entries"
        )

        require(
          multiAsset.values.forall(_.forall { case (_, amount) => amount != 0 }),
          "MultiAsset cannot contain zeros"
        )
    }
}

object Value:
    /** Zero value (0 ADA, no assets) */
    val zero: Value = Value(Coin.zero)

    /** Create a pure ADA value */
    def lovelace(amount: Long): Value = Value(Coin(amount))

    /** Create a Value from coin and multi-asset */
    def apply(coin: Coin, multiAsset: ledger.MultiAsset = Map.empty): Value =
        new Value(coin, multiAsset)

    /** CBOR encoder for Value */
    given Encoder[Value] with
        def write(w: Writer, value: Value): Writer =
            if value.assets.isEmpty then
                // Pure ADA value
                w.write(value.coin)
            else
                // Multi-asset value
                w.writeArrayHeader(2) // 2 elements: coin and multi-asset
                w.write(value.coin)
                writeMultiAsset(w, value.assets)

    /** Helper method to write MultiAsset as CBOR */
    private def writeMultiAsset(
        w: Writer,
        multiAsset: ledger.MultiAsset
    ): Writer =
        // Write the map header with number of policies
        w.writeMapHeader(multiAsset.size)

        // Write each policy and its assets
        multiAsset.foreach { case (policyId, assets) =>
            // Write policy ID
            w.write(policyId)

            // Write asset map
            w.writeMapHeader(assets.size)
            assets.foreach { case (assetName, amount) =>
                w.write(assetName)
                w.writeLong(amount)
            }
        }
        w

    /** CBOR decoder for Value */
    given Decoder[Value] with
        def read(r: Reader): Value =
            if r.hasArrayHeader then
                val size = r.readArrayHeader()
                if size != 2 then
                    r.validationFailure(s"Expected 2 elements for MultiAssetValue, got $size")

                val coin = r.read[Coin]()
                val multiAsset = r.read[ledger.MultiAsset]()
                new Value(coin, multiAsset)
            else
                // Single coin value
                val coin = r.read[Coin]()
                Value(coin)
