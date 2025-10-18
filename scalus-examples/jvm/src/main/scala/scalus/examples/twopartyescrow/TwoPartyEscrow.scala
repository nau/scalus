package scalus.examples.twopartyescrow

import scalus.*
import scalus.Compiler.compile
import scalus.builtin.Data.{FromData, ToData}
import scalus.builtin.{ByteString, Data}
import scalus.cardano.blueprint.{Application, Blueprint}
import scalus.ledger.api.v1.{Interval, IntervalBoundType, PosixTime}
import scalus.ledger.api.v1.Value.getLovelace
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.uplc.Program

type PubKeyHash = ByteString

/** Escrow state transitions
  *   - Initial → Deposited (via Deposit action)
  *   - Deposited → Accepted (via Accept action, seller accepts funds)
  *   - Deposited → Refunded (via Refund action, buyer reclaims after deadline)
  */
enum EscrowState derives FromData, ToData:
    case Initial
    case Deposited
    case Accepted
    case Refunded

@Compile
object EscrowState

/** Escrow datum containing state, timestamps, and participant information
  *
  * @param state
  *   Current state of the escrow
  * @param depositTime
  *   Timestamp when funds were deposited
  * @param buyerKeyHash
  *   Public key hash of the buyer
  * @param sellerKeyHash
  *   Public key hash of the seller
  */
case class EscrowDatum(
    state: EscrowState,
    depositTime: PosixTime,
    buyerKeyHash: PubKeyHash,
    sellerKeyHash: PubKeyHash,
    escrowPrice: BigInt,
    refundTime: PosixTime
) derives FromData,
      ToData

@Compile
object EscrowDatum

/** Redeemer actions for escrow state transitions */
enum Action derives FromData, ToData:
    case Deposit
    case Accept
    case Refund

@Compile
object Action

/** Two-Party Escrow Validator
  *
  * A simple escrow contract for two parties (buyer and seller):
  *   - Buyer deposits funds (Deposit)
  *   - Seller can accept the funds before refund deadline (Accept)
  *   - Buyer can reclaim funds after refund deadline (Refund)
  *
  * State transitions:
  *   - Deposit: Initial → Deposited (creates escrow with Deposited state)
  *   - Accept: Deposited → Accepted (seller accepts, funds go to seller)
  *   - Refund: Deposited → Refunded (buyer reclaims after deadline)
  */
@Compile
object TwoPartyEscrow extends Validator:
    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        redeemer.to[Action] match
            case Action.Deposit => validateDeposit(tx, ownRef)
            case Action.Accept  => validateAccept(datum, tx, ownRef)
            case Action.Refund  => validateRefund(datum, tx, ownRef)
    }

    /** Validates buyer deposit operation, creating escrow UTXO with Deposited state */
    def validateDeposit(tx: TxInfo, ownRef: TxOutRef): Unit = {
        val scriptOuts = getScriptOutputs(tx, ownRef)
        require(scriptOuts.size === BigInt(1), NoScriptOutputsCreated)

        val onlyOut = scriptOuts.head
        val validRange = tx.validRange

        // Verify deposit datum is valid
        onlyOut.datum match
            case ledger.api.v2.OutputDatum.OutputDatum(d) =>
                val escrowDatum = d.to[EscrowDatum]
                require(escrowDatum.state.isDeposited, InvalidDepositState)

                // Verify deposit time matches current transaction time
                val currentTime = validRange.from.boundType match
                    case IntervalBoundType.Finite(time) => time
                    case _                              => fail(NoValidTimeRange)

                require(escrowDatum.depositTime === currentTime, InvalidDepositTime)

                // Verify buyer signature
                require(tx.isSignedBy(escrowDatum.buyerKeyHash), BuyerSignatureMissing)

                // Verify correct amount in script output
                require(
                  onlyOut.value.getLovelace === escrowDatum.escrowPrice,
                  WrongScriptOutputAmount
                )

            case _ => fail(NoDatumProvided)
    }

    /** Validates seller accept operation, paying escrow funds to seller */
    def validateAccept(datum: Option[Data], tx: TxInfo, ownRef: TxOutRef): Unit = {
        val escrowDatum = datum.map(_.to[EscrowDatum]).getOrFail(InvalidDatum)

        // Verify current state is Deposited
        require(escrowDatum.state.isDeposited, AcceptOnlyFromDeposited)

        // Verify seller signature
        require(tx.isSignedBy(escrowDatum.sellerKeyHash), SellerSignatureMissing)

        // Verify no continuing outputs (complete withdrawal)
        val scriptOuts = getContinuingOutputs(tx, ownRef)
        require(scriptOuts.size === BigInt(0), IncompleteWithdrawal)

        // Verify escrow input exists and has correct amount
        require(hasEscrowInput(tx.inputs, escrowDatum.escrowPrice), NoValidEscrowInput)

        // Verify correct payment to seller
        require(
          escrowValuePaidTo(tx, escrowDatum.sellerKeyHash, escrowDatum.escrowPrice),
          IncorrectPaymentToSeller
        )
    }

    /** Validates buyer refund operation, returning escrow funds to buyer after deadline */
    def validateRefund(datum: Option[Data], tx: TxInfo, ownRef: TxOutRef): Unit = {
        val escrowDatum = datum.map(_.to[EscrowDatum]).getOrFail(InvalidDatum)

        // Verify current state is Deposited
        require(escrowDatum.state.isDeposited, RefundOnlyFromDeposited)

        // Verify buyer signature
        require(tx.isSignedBy(escrowDatum.buyerKeyHash), BuyerSignatureMissing)

        // Verify refund time has been reached
        val refundDeadline = escrowDatum.depositTime + escrowDatum.refundTime + BigInt(1)
        require(tx.validRange.isAfter(refundDeadline), RefundTimeNotReached)

        // Verify escrow input exists and has correct amount
        require(hasEscrowInput(tx.inputs, escrowDatum.escrowPrice), NoValidEscrowInput)

        // Verify correct refund to buyer
        require(
          escrowValuePaidTo(tx, escrowDatum.buyerKeyHash, escrowDatum.escrowPrice),
          IncorrectRefundToBuyer
        )
    }

    // Helper functions

    /** Filters transaction outputs to only those sent to the escrow script address */
    def getScriptOutputs(tx: TxInfo, ownRef: TxOutRef): scalus.prelude.List[TxOut] = {
        val ownInput = tx.findOwnInput(ownRef).getOrFail(OwnInputNotFound)
        val scriptAddr = ownInput.resolved.address
        tx.outputs.filter(out => out.address === scriptAddr)
    }

    /** Gets continuing outputs (outputs to the same script) */
    def getContinuingOutputs(tx: TxInfo, ownRef: TxOutRef): scalus.prelude.List[TxOut] = {
        val ownInput = tx.findOwnInput(ownRef).getOrFail(OwnInputNotFound)
        tx.outputs.filter(out => out.address.credential === ownInput.resolved.address.credential)
    }

    /** Checks if transaction has a valid escrow input with correct amount */
    def hasEscrowInput(inputs: scalus.prelude.List[TxInInfo], expectedAmount: BigInt): Boolean =
        inputs.exists { input =>
            input.resolved.address.credential match
                case ledger.api.v1.Credential.ScriptCredential(_) =>
                    input.resolved.value.getLovelace === expectedAmount
                case _ => false
        }

    /** Checks if escrow amount was paid to the specified key hash */
    def escrowValuePaidTo(tx: TxInfo, keyHash: PubKeyHash, expectedAmount: BigInt): Boolean = {
        val totalPaid = tx.outputs
            .filter(out => addressMatchesPubKeyHash(out.address, keyHash))
            .foldLeft(BigInt(0))((acc, out) => acc + out.value.getLovelace)
        totalPaid >= expectedAmount
    }

    /** Checks if an address matches a public key hash */
    def addressMatchesPubKeyHash(addr: ledger.api.v1.Address, pkh: PubKeyHash): Boolean =
        addr.credential match
            case ledger.api.v1.Credential.PubKeyCredential(hash) => hash.hash === pkh
            case _                                               => false

    // Extension methods

    extension (self: TxInfo)
        def isSignedBy(pubKeyHash: PubKeyHash): Boolean =
            self.signatories.exists(_.hash === pubKeyHash)

    extension (self: Interval)
        infix def isAfter(timePoint: PosixTime): Boolean =
            self.from.boundType match
                case IntervalBoundType.Finite(time) => timePoint < time
                case _                              => false

    extension (s: EscrowState)
        def isDeposited: Boolean = s match
            case EscrowState.Deposited => true
            case _                     => false

    // Error messages
    inline val InvalidDatum = "Datum must be a valid EscrowDatum"
    inline val NoScriptOutputsCreated = "Deposit must create exactly one script output"
    inline val TooManyScriptOutputs = "Too many script outputs created"
    inline val BuyerSignatureMissing = "Buyer signature missing"
    inline val SellerSignatureMissing = "Seller signature missing"
    inline val WrongScriptOutputAmount = "Wrong script output amount"
    inline val InvalidDepositState = "Deposit datum must have Deposited state"
    inline val InvalidDepositTime = "Invalid deposit time in datum"
    inline val NoValidTimeRange = "No valid time range provided"
    inline val NoDatumProvided = "No datum provided in output"
    inline val AcceptOnlyFromDeposited = "Accept only valid from Deposited state"
    inline val RefundOnlyFromDeposited = "Refund only valid from Deposited state"
    inline val IncompleteWithdrawal = "Incomplete withdrawal - funds remain in script"
    inline val NoValidEscrowInput = "No valid escrow deposit found in inputs"
    inline val IncorrectPaymentToSeller = "Incorrect payment to seller"
    inline val IncorrectRefundToBuyer = "Incorrect refund to buyer"
    inline val RefundTimeNotReached = "Refund time not reached"
    inline val OwnInputNotFound = "Own input not found"

    @Ignore
    given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplc110Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

    @Ignore
    val script: Program = {
        val sir = compile(TwoPartyEscrow.validate)
        sir.toUplc(
          generateErrorTraces = true,
          backend = scalus.Compiler.TargetLoweringBackend.SirToUplc110Lowering
        ).plutusV3
    }

end TwoPartyEscrow

object TwoPartyEscrowContract:
    val application: Application = {
        Application.ofSingleValidator[EscrowDatum, Action](
          "Two-Party Escrow",
          "Simple escrow contract for two parties with deposit, accept, and refund operations",
          "1.0.0",
          TwoPartyEscrow.validate
        )
    }

    val blueprint: Blueprint = application.blueprint

end TwoPartyEscrowContract
