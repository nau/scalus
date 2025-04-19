package scalus.examples

import scalus.*
import scalus.builtin.Data
import scalus.builtin.Data.{FromData, ToData}
import scalus.builtin.{ByteString, FromData, ToData}
import scalus.builtin.Builtins.sha3_256
import scalus.prelude.{*, given}
import scalus.builtin.FromDataInstances.given
import scalus.builtin.ToDataInstances.given
import scalus.ledger.api.v3.{Interval, IntervalBoundType, PosixTime, TxInfo, TxOutRef}

@Compile
object HtlcValidator extends Validator:
    private type Preimage = ByteString
    private type Image = ByteString
    private type PubKeyHash = ByteString

    case class ContractDatum(
        benefactor: PubKeyHash,
        beneficiary: PubKeyHash,
        image: Image,
        timeout: PosixTime
    )

    enum ContractRedeemer:
        case Benefactor
        case Beneficiary(preimage: Preimage)

    override def spend(
        datum: Option[Data],
        redeemer: Data,
        targetTxInfo: TxInfo,
        sourceTxOutRef: TxOutRef
    ): Unit =
        val ContractDatum(benefactor, beneficiary, image, timeout) =
            datum.map(_.to[ContractDatum]).getOrFail(InvalidDatum)

        redeemer.to[ContractRedeemer] match
            case ContractRedeemer.Benefactor =>
                targetTxInfo.isSignedBy(benefactor) orFail UnsignedBenefactorTransaction
                targetTxInfo.validRange.isAfter(timeout) orFail InvalidBenefactorTimePoint

            case ContractRedeemer.Beneficiary(preimage) =>
                targetTxInfo.isSignedBy(beneficiary) orFail UnsignedBeneficiaryTransaction
                !targetTxInfo.validRange.isAfter(timeout) orFail InvalidBeneficiaryTimePoint
                sha3_256(preimage) === image orFail InvalidPreimage

    given FromData[ContractDatum] = FromData.deriveCaseClass[ContractDatum]
    given ToData[ContractDatum] = ToData.deriveCaseClass[ContractDatum](0)
    given FromData[ContractRedeemer] = FromData.deriveEnum[ContractRedeemer]
    given ToData[ContractRedeemer] = ToData.deriveEnum[ContractRedeemer]

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

    inline val InvalidDatum =
        "Datum must be a HtlcValidator.ContractDatum(benefactor, beneficiary, image, timeout)"
    inline val UnsignedBenefactorTransaction = "Transaction must be signed by a benefactor"
    inline val UnsignedBeneficiaryTransaction =
        "Transaction must be signed by a beneficiary"
    inline val InvalidBenefactorTimePoint =
        "Benefactor Transaction must be exclusively after timeout"
    inline val InvalidBeneficiaryTimePoint =
        "Beneficiary Transaction must be inclusively before timeout"
    inline val InvalidPreimage = "Invalid preimage"

end HtlcValidator
