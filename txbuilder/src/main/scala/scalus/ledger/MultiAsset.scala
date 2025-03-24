package scalus.ledger

import io.bullet.borer.{Decoder, Encoder, Reader, Writer}

/** Represents a collection of native assets in Cardano
  *
  * @tparam A
  *   The type of values in the assets map (typically Long)
  */
type MultiAsset[A] = Map[PolicyId, Map[AssetName, A]]

object MultiAsset:
    /** CBOR encoder for MultiAsset */
    given [A: Encoder]: Encoder[MultiAsset[A]] with
        def write(w: Writer, value: MultiAsset[A]): Writer =
            w.writeMapHeader(value.size)

            value.foreach { case (policyId, assets) =>
                // Write policy ID
                w.write(policyId)

                // Write asset map
                w.writeMapHeader(assets.size)
                assets.foreach { case (assetName, amount) =>
                    AssetName.given_Encoder_AssetName.write(w, assetName)
                    summon[Encoder[A]].write(w, amount)
                }
            }
            w

    /** CBOR decoder for MultiAsset */
    given [A: Decoder]: Decoder[MultiAsset[A]] with
        def read(r: Reader): MultiAsset[A] =
            val policyCount = r.readMapHeader()
            val multiAssetBuilder = Map.newBuilder[PolicyId, Map[AssetName, A]]

            for _ <- 0L until policyCount do
                val policyId = r.read[PolicyId]()

                val assetCount = r.readMapHeader()
                val assetsBuilder = Map.newBuilder[AssetName, A]

                for _ <- 0L until assetCount do
                    val assetName = r.read[AssetName]()
                    val amount = summon[Decoder[A]].read(r)
                    assetsBuilder += (assetName -> amount)

                multiAssetBuilder += (policyId -> assetsBuilder.result())

            multiAssetBuilder.result()

type Mint = MultiAsset[Long]