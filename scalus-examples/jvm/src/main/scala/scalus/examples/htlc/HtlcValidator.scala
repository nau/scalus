package scalus.examples.htlc

import scalus.*
import scalus.Compiler.compile
import scalus.builtin.Builtins.sha3_256
import scalus.builtin.Data.{FromData, ToData}
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.cardano.blueprint.{Application, Blueprint}
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.uplc.Program

type Preimage = ByteString
type Image = ByteString
type PubKeyHash = ByteString

// Contract Datum
case class ContractDatum(
    committer: PubKeyHash,
    receiver: PubKeyHash,
    image: Image,
    timeout: PosixTime
) derives FromData,
      ToData

// Redeemer
enum Action derives FromData, ToData:
    case Timeout
    case Reveal(preimage: Preimage)

@Compile
object HtlcValidator extends Validator:
    /** Spending script purpose validation
      */
    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val ContractDatum(committer, receiver, image, timeout) =
            datum.map(_.to[ContractDatum]).getOrFail(InvalidDatum)

        redeemer.to[Action] match
            case Action.Timeout =>
                require(tx.isSignedBy(committer), UnsignedCommitterTransaction)
                require(tx.validRange.isEntirelyAfter(timeout), InvalidCommitterTimePoint)

            case Action.Reveal(preimage) =>
                require(tx.isSignedBy(receiver), UnsignedReceiverTransaction)
                require(!tx.validRange.isEntirelyAfter(timeout), InvalidReceiverTimePoint)
                require(sha3_256(preimage) === image, InvalidReceiverPreimage)
    }

    // Error messages
    inline val InvalidDatum =
        "Datum must be a HtlcValidator.ContractDatum(committer, receiver, image, timeout)"
    inline val UnsignedCommitterTransaction = "Transaction must be signed by a committer"
    inline val UnsignedReceiverTransaction = "Transaction must be signed by a receiver"
    inline val InvalidCommitterTimePoint = "Committer Transaction must be exclusively after timeout"
    inline val InvalidReceiverTimePoint = "Receiver Transaction must be inclusively before timeout"
    inline val InvalidReceiverPreimage = "Invalid receiver preimage"

    @Ignore
    given scalus.Compiler.Options = scalus.Compiler.Options(
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
