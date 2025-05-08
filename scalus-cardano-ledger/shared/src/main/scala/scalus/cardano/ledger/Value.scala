package scalus.cardano.ledger

import io.bullet.borer.{Decoder, Encoder, Reader, Writer}

/** Represents a value in Cardano, which can be either pure ADA or ADA with multi-assets */
enum Value:
    /** Pure ADA value with no multi-assets */
    case Ada(coin: Coin)

    /** ADA value with multi-assets */
    case MultiAsset(
        coin: Coin,
        assets: Map[PolicyId, Map[AssetName, Long]]
    )

    /** Get the multi-asset component if present, empty otherwise */
    def multiAsset: Map[PolicyId, Map[AssetName, Long]] = this match
        case Ada(_)                => Map.empty
        case MultiAsset(_, assets) => assets

object Value:
    /** Zero value (0 ADA, no assets) */
    val zero: Value = Value.Ada(Coin.zero)

    /** Create a pure ADA value */
    def lovelace(amount: Long): Value = Value.Ada(Coin(amount))

    /** Create a Value from coin and multi-asset */
    def apply(coin: Coin, multiAsset: Map[PolicyId, Map[AssetName, Long]] = Map.empty): Value =
        if multiAsset.isEmpty then Value.Ada(coin)
        else
            // Validate multi-asset map
            validateMultiAsset(multiAsset)
            Value.MultiAsset(coin, multiAsset)

    /** Validate a multi-asset map according to Cardano rules */
    private def validateMultiAsset(multiAsset: Map[PolicyId, Map[AssetName, Long]]): Unit =
        // Validate that all policy maps are non-empty
        require(
          multiAsset.forall { case (_, assets) => assets.nonEmpty },
          "Multi-asset map cannot contain empty policy entries"
        )

        // Validate that all asset values are positive
        require(
          multiAsset.forall { case (_, assets) =>
              assets.forall { case (_, value) => value > 0 }
          },
          "Multi-asset values must be positive"
        )

    /** CBOR encoder for Value */
    given Encoder[Value] with
        def write(w: Writer, value: Value): Writer = value match
            case Value.Ada(coin) =>
                w.write(coin)
            case Value.MultiAsset(coin, multiAsset) =>
                w.writeArrayHeader(2)
                w.write(coin)
                writeMultiAsset(w, multiAsset)

    /** Helper method to write MultiAsset as CBOR */
    private def writeMultiAsset(
        w: Writer,
        multiAsset: Map[PolicyId, Map[AssetName, Long]]
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
                val multiAsset = r.read[Map[PolicyId, Map[AssetName, Long]]]()
                Value.MultiAsset(coin, multiAsset)
            else
                // Single coin value
                val coin = r.read[Coin]()
                Value.Ada(coin)
