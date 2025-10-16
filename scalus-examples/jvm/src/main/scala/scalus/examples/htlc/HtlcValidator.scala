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
                require(tx.validRange.isAfter(timeout), InvalidCommitterTimePoint)

            case Action.Reveal(preimage) =>
                require(tx.isSignedBy(receiver), UnsignedReceiverTransaction)
                require(!tx.validRange.isAfter(timeout), InvalidReceiverTimePoint)
                require(sha3_256(preimage) === image, InvalidReceiverPreimage)
    }

    // Helper methods
    extension (self: TxInfo)
        def isSignedBy(pubKeyHash: PubKeyHash): Boolean =
            self.signatories.exists { _.hash === pubKeyHash }

    extension (self: Interval)
        infix def isAfter(timePoint: PosixTime): Boolean =
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

// 1. Make PlutusV3.create
// 2. Make compiled() with options
// 3. Make options Debug/Release
// 4. Make evaluate(ctx)
object HtlcContract:

    val contract = PlutusV3.create[ContractDatum, Action](
      title = "HTLC Validator",
      description =
          "Releases funds when recipient reveals hash preimage before deadline, otherwise refunds to sender.",
      version = "1.0.0",
    )(HtlcValidator)
//
    val compiled = contract.compiled(Options.Debug = Debug)
    val compiled = contract.compiled(Options.Release)
    val compiled = contract.compiled(Options.Release).evaluate(ctx)
    val ctx = tx.toScriptContext(ownRef)
    val compiled = contract.compiled(Options.Release)

//    compiled.script: Script.PlutusV3
//    compiled.blueprint: Blueprint
//    compiled.program: Program

end HtlcContract

// Rename backends:
given Debug: scalus.Compiler.Options = scalus.Compiler.Options(
  targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
  generateErrorTraces = true,
  optimizeUplc = false,
  debug = false
)

given Release: scalus.Compiler.Options = scalus.Compiler.Options(
  targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
  generateErrorTraces = false,
  optimizeUplc = true,
  debug = false
)

txbuilder.payTo(HtlcContract.compiled, datum: ContractDatum)
