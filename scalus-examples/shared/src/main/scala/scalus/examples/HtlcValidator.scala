package scalus.examples

import scalus.*
import scalus.builtin.Data
import scalus.builtin.ByteString
import scalus.builtin.FromDataInstances.given
import scalus.ledger.api.v3.{TxInfo, TxOutRef}
import scalus.prelude.{*, given}

@Compile
object HtlcValidator extends Validator:
    private type Preimage = ByteString
    private type Image = ByteString
    private type Signature = ByteString

    override def spend(
        datum: Option[Data],
        redeemer: Data,
        targetTxInfo: TxInfo,
        sourceTxOutRef: TxOutRef
    ): Unit =
        val (image, signature) = datum.map(_.toImageAndSignature).getOrFail(InvalidDatum)
        targetTxInfo.isSigned(signature) orFail UnsignedTransaction
        redeemer.toPreimage.sha2_256 === image orFail WrongPreimage

    extension (self: Data)
        private inline def toImageAndSignature: (Image, Signature) = self.to[(Image, Signature)]
        private inline def toPreimage: Preimage = self.toByteString

    extension (self: TxInfo)
        private def isSigned(
            signature: Signature
        ): Boolean = self.signatories.exists { _.hash === signature }

    extension (self: Preimage)
        private inline def sha2_256: Image = scalus.builtin.Builtins.sha2_256(self)

    private inline val InvalidDatum = "Datum must be a tuple of (image, signature)"
    private inline val UnsignedTransaction = "Transaction must be signed"
    private inline val WrongPreimage = "Wrong preimage"

end HtlcValidator
