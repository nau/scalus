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
        committer: PubKeyHash,
        receiver: PubKeyHash,
        image: Image,
        timeout: PosixTime
    )

    enum Action:
        case Timeout
        case Reveal(preimage: Preimage)

    override def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit =
        val ContractDatum(committer, receiver, image, timeout) =
            datum.map(_.to[ContractDatum]).getOrFail(InvalidDatum)

        redeemer.to[Action] match
            case Action.Timeout =>
                tx.isSignedBy(committer) orFail UnsignedCommitterTransaction
                tx.validRange.isAfter(timeout) orFail InvalidCommitterTimePoint

            case Action.Reveal(preimage) =>
                tx.isSignedBy(receiver) orFail UnsignedReceiverTransaction
                !tx.validRange.isAfter(timeout) orFail InvalidReceiverTimePoint
                sha3_256(preimage) === image orFail InvalidReceiverPreimage

    given FromData[ContractDatum] = FromData.deriveCaseClass[ContractDatum]
    given ToData[ContractDatum] = ToData.deriveCaseClass[ContractDatum](0)
    given FromData[Action] = FromData.deriveEnum[Action]
    given ToData[Action] = ToData.deriveEnum[Action]

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
        "Datum must be a HtlcValidator.ContractDatum(committer, receiver, image, timeout)"
    inline val UnsignedCommitterTransaction = "Transaction must be signed by a committer"
    inline val UnsignedReceiverTransaction = "Transaction must be signed by a receiver"
    inline val InvalidCommitterTimePoint = "Committer Transaction must be exclusively after timeout"
    inline val InvalidReceiverTimePoint = "Receiver Transaction must be inclusively before timeout"
    inline val InvalidReceiverPreimage = "Invalid receiver preimage"

end HtlcValidator
