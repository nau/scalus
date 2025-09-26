package scalus.regression.hydrozoa20250804

import scalus.*
import scalus.builtin.{Data, FromData}
import scalus.ledger.api.v2.OutputDatum.OutputDatum
import scalus.ledger.api.v3.TxOut

@Compile
object TxOutExtensions {
    extension (self: TxOut)
        /** Returns inline datum of type T of fails.
          *
          * @param x$1
          * @tparam T
          * @return
          */
        def inlineDatumOfType[T](using FromData[T]): T =
            val OutputDatum(inlineDatum) = self.datum: @unchecked
            inlineDatum.to[T]

}
