package scalus.cardano.ledger

import cats.kernel.CommutativeGroup
import io.bullet.borer.{Decoder, Encoder, Reader, Writer}
import scalus.cardano.ledger

// FIXME: make sure we validate Value is non-negative in ledger rules
/** Represents a value in Cardano, which can be either pure ADA or ADA with multi-assets */
case class Value(coin: Coin, assets: ledger.MultiAsset) {
    // Validate multi-asset map
    validateMultiAsset(assets)

    infix def +(rhs: Value): Value = binOp(_ + _)(rhs)

    infix def -(rhs: Value): Value = binOp(_ - _)(rhs)

    def unary_- : Value =
        Value(Coin(-coin.value), summon[CommutativeGroup[MultiAsset]].inverse(assets))

    def isPositive: Boolean = coin.value > 0 && assets.isPositive

    def isNegative: Boolean = coin.value < 0 && assets.isNegative

    def isZero: Boolean = coin.value == 0 && assets.isEmpty

    /** Validate a multi-asset map according to Cardano rules */
    private def validateMultiAsset(multiAsset: ledger.MultiAsset): Unit = {
        // Validate that all policy maps are non-empty
        require(
          multiAsset.assets.forall { case (_, assets) => assets.nonEmpty },
          "MultiAsset map cannot contain empty policy entries"
        )

        require(
          multiAsset.assets.values.forall(_.forall { case (_, amount) => amount != 0 }),
          "MultiAsset cannot contain zeros"
        )
    }

    private def binOp(op: (Long, Long) => Long)(rhs: Value): Value = (this, rhs) match
        case (Value(coin1, assets1), Value(coin2, assets2)) =>
            Value(Coin(op(coin1.value, coin2.value)), ledger.MultiAsset.binOp(op)(assets1, assets2))
}

object Value:
    /** Zero value (0 ADA, no assets) */
    val zero: Value = Value(Coin.zero)

    /** Create a pure ADA value */
    def lovelace(amount: Long): Value = Value(Coin(amount))

    /** Create a Value from coin and multi-asset */
    def apply(coin: Coin, multiAsset: ledger.MultiAsset = MultiAsset.empty): Value =
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
                w.write(value.assets)

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

    given CommutativeGroup[Value] with
        def combine(x: Value, y: Value): Value = x + y
        def empty: Value = Value.zero
        def inverse(x: Value): Value = -x
