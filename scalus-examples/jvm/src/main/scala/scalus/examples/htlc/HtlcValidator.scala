package scalus.examples.htlc

import scalus.*
import scalus.Compiler.compile
import scalus.builtin.Builtins.sha3_256
import scalus.builtin.Data.{FromData, ToData}
import scalus.builtin.*
import scalus.cardano.blueprint.{Application, PlutusV3}
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
    override def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit = {
        val ContractDatum(committer, receiver, image, timeout) =
            datum.map(_.to[ContractDatum]).getOrFail(InvalidDatum)

        redeemer.to[Action] match
            case Action.Timeout =>
                require(tx.isSignedBy(committer), UnsignedCommitterTransaction)
                require(tx.validRange.isEntirelyAfter(timeout), InvalidCommitterTimePoint)

            case Action.Reveal(preimage) =>
                require(tx.isSignedBy(receiver), UnsignedReceiverTransaction)
                require(!tx.validRange.isEntirelyAfter(timeout), InvalidReceiverTimePoint)
                require(sha3_256(preimage) == image, InvalidReceiverPreimage)
    }

    // Helper methods
    extension (self: TxInfo)
        private def isSignedBy(pubKeyHash: PubKeyHash): Boolean =
            self.signatories.exists { _.hash == pubKeyHash }

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

object Htlc:
    val contract = PlutusV3.create[ContractDatum, Action](
      title = "Hashed timelocked contract",
      description =
          "Releases funds when recipient reveals hash preimage before deadline, otherwise refunds to sender."
    )(HtlcValidator.validate)

    val application = Application(
      title = "Hashed timelocked contract",
      description =
          "Releases funds when recipient reveals hash preimage before deadline, otherwise refunds to sender.",
      version = "1.0.0",
      contracts = Seq(contract)
    )
end Htlc

// validate(ctx: ScriptContext): Unit => SIR
// SIR ?=> (Debug, Release Traces, Backend, Optimization) UPLC
// UPLC => optimize => UPLC
// => (v1,v2,v3,v4) Program
// => PlutusScript => // Cardano Client Lib integration helper: CCL.PlutusScript helper function
// => CBOR(FLAT)
// Blueprint.Validator (compiled CBOR, Datum/Redeemer schemas, description)
// Blueprint.Validator => Blueprint (contracts: List[Validator], preamble: Preamble)
// Application (blueprint: Blueprint) => JSON (CIP-57) => CLI => API

// Debug/Release Error handling
// Custom Optimizations
// Backend selection
// Blueprint
// Application (multiple contracts, endpoints, tx building, testing, deployment, description)
