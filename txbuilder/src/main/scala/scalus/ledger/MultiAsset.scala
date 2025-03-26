package scalus.ledger

/** Represents a collection of native assets in Cardano
  *
  * @tparam A
  *   The type of values in the assets map (typically Long)
  */
type MultiAsset[A] = Map[PolicyId, Map[AssetName, A]]

type Mint = MultiAsset[Long]