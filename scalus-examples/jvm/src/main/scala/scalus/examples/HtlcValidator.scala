package scalus.examples

import scalus.*
import scalus.Compiler.compile
import scalus.builtin.Builtins.sha3_256
import scalus.builtin.Data.{FromData, ToData}
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.cardano.plutus.contract.blueprint.{Application, Blueprint}
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.uplc.Program

@Compile
object HtlcValidator extends Validator:
    private type Preimage = ByteString
    private type Image = ByteString
    private type PubKeyHash = ByteString

    // Contract Datum
    case class ContractDatum(
        committer: PubKeyHash,
        receiver: PubKeyHash,
        image: Image,
        timeout: PosixTime
    )

    // Redeemer
    enum Action:
        case Timeout
        case Reveal(preimage: Preimage)

    // Data converters for ContractDatum and Action
    // used in test and transaction building offchain logic
    given FromData[ContractDatum] = FromData.derived
    given ToData[ContractDatum] = ToData.derived
    given FromData[Action] = FromData.derived
    given ToData[Action] = ToData.derived

    // val d = Action.Timeout.toData

    /** Spending script purpose validation
      */
    override def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit = {
        // Read ContractDatum from datum
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
    }

    // Helper methods
    extension (self: TxInfo)
        private def isSignedBy(pubKeyHash: PubKeyHash): Boolean =
            self.signatories.exists { _.hash === pubKeyHash }

    extension (self: Interval)
        private infix def isAfter(timePoint: PosixTime): Boolean =
            self.from.boundType match
                case IntervalBoundType.Finite(time) => timePoint < time
                case _                              => false

    // Error messages
    inline val InvalidDatum =
        "Datum must be a HtlcValidator.ContractDatum(committer, receiver, image, timeout)"
    inline val UnsignedCommitterTransaction = "Transaction must be signed by a committer"
    inline val UnsignedReceiverTransaction = "Transaction must be signed by a receiver"
    inline val InvalidCommitterTimePoint = "Committer Transaction must be exclusively after timeout"
    inline val InvalidReceiverTimePoint = "Receiver Transaction must be inclusively before timeout"
    inline val InvalidReceiverPreimage = "Invalid receiver preimage"

    @Ignore
    inline given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

    @Ignore
    val script: Program = {
        val sir = compile(HtlcValidator.validate)
        // val lw = sir.toLoweredValue()
        // println(lw.show)
        sir.toUplc(
          generateErrorTraces = true,
          backend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering
        ).plutusV3
    }

end HtlcValidator

object HtlcContract:

    import scalus.examples.HtlcValidator.{Action, ContractDatum}
    val application: Application = {
        Application.ofSingleValidator[ContractDatum, Action](
          "Hashed timelocked contract",
          "Releases funds when recipient reveals hash preimage before deadline, otherwise refunds to sender.",
          "1.0.0",
          HtlcValidator.validate
        )
    }

    val bluerprint: Blueprint = application.blueprint

end HtlcContract
