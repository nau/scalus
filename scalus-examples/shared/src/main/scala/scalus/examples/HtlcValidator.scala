package scalus.examples

import scalus.*
import scalus.builtin.Data
import scalus.builtin.Data.{FromData, ToData}
import scalus.builtin.{ByteString, FromData, ToData}
import scalus.builtin.Builtins.sha3_256
import scalus.prelude.{*, given}
import scalus.builtin.FromDataInstances.given
import scalus.builtin.ToDataInstances.given
import scalus.ledger.api.v3.given
import scalus.ledger.api.v3.{Interval, IntervalBoundType, PosixTime, TxInfo, TxOutRef}

@Compile
object HtlcValidator extends Validator:
    type Preimage = ByteString
    type Image = ByteString
    type PubKeyHash = ByteString

    case class Contract(
        benefactor: PubKeyHash,
        beneficiary: PubKeyHash,
        image: Image,
        timeout: PosixTime
    )

    enum Redeemer:
        case Benefactor
        case Beneficiary(preimage: Preimage)

    override def spend(
        datum: Option[Data],
        redeemer: Data,
        targetTxInfo: TxInfo,
        sourceTxOutRef: TxOutRef
    ): Unit =
        val Contract(benefactor, beneficiary, image, timeout) =
            datum.map(_.to[Contract]).getOrFail(InvalidDatum)

        redeemer.to[Redeemer] match
            case Redeemer.Benefactor =>
                targetTxInfo.isSignedBy(benefactor) orFail UnsignedBenefactorTransaction
                targetTxInfo.validRange.isAfter(timeout) orFail InvalidBenefactorTimePoint

            case Redeemer.Beneficiary(preimage) =>
                targetTxInfo.isSignedBy(beneficiary) orFail UnsignedBeneficiaryTransaction
                !targetTxInfo.validRange.isAfter(timeout) orFail InvalidBeneficiaryTimePoint
                preimage.sha3_256 === image orFail InvalidPreimage

    given FromData[Contract] = FromData.deriveCaseClass[Contract]
    given ToData[Contract] = ToData.deriveCaseClass[Contract](0)
    given FromData[Redeemer] = FromData.deriveEnum[Redeemer]
    given ToData[Redeemer] = ToData.deriveEnum[Redeemer]

    extension (self: TxInfo)
        private def isSignedBy(
            pubKeyHash: PubKeyHash
        ): Boolean = self.signatories.exists { _.hash === pubKeyHash }

    extension (self: Interval)
        private def isAfter(
            timePoint: PosixTime
        ): Boolean = self.from.boundType match
            case IntervalBoundType.Finite(time) => timePoint < time
            case _                              => false

    private inline val InvalidDatum =
        "Datum must be a HtlcValidator.Contract(benefactor, beneficiary, image, timeout)"
    private inline val UnsignedBenefactorTransaction = "Transaction must be signed by a benefactor"
    private inline val UnsignedBeneficiaryTransaction =
        "Transaction must be signed by a beneficiary"
    private inline val InvalidBenefactorTimePoint =
        "Benefactor Transaction must be exclusively after timeout"
    private inline val InvalidBeneficiaryTimePoint =
        "Beneficiary Transaction must be inclusively before timeout"
    private inline val InvalidPreimage = "Invalid preimage"

end HtlcValidator
