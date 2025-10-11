package scalus.examples

import scalus.{show as _, *}
import scalus.prelude.Show.*
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.prelude.Option.*
import scalus.ledger.api.v2.OutputDatum
import scalus.builtin.Data
import scalus.builtin.Data.{FromData, ToData}
import scalus.ledger.api.v1.Value.getLovelace
import scalus.Compiler.compile
import scalus.builtin.Builtins.trace

case class EscrowDatum(
    seller: PubKeyHash,
    buyer: PubKeyHash,
    escrowAmount: Lovelace,
    initializationAmount: Lovelace
) derives FromData,
      ToData

@Compile
object EscrowDatum {
    given Eq[EscrowDatum] = (x, y) =>
        x.buyer === y.buyer && x.seller === y.seller &&
            x.escrowAmount === y.escrowAmount && x.initializationAmount === y.initializationAmount
}

enum EscrowAction derives FromData, ToData:
    case Deposit
    case Pay
    case Refund

case class EscrowRedeemer(action: EscrowAction) derives FromData, ToData

@Compile
object EscrowRedeemer

@Compile
object EscrowUtils {

    def getOutputsByVkh(outputs: List[TxOut], vkh: PubKeyHash): List[TxOut] = {
        outputs.filter(output =>
            output.address.credential match {
                case Credential.PubKeyCredential(pkh) => pkh === vkh
                case _                                => false
            }
        )
    }

    def getOutputsByAddress(outputs: List[TxOut], addr: Address): List[TxOut] = {
        outputs.filter(_.address === addr)
    }

    def getInputsByAddress(inputs: List[TxInInfo], addr: Address): List[TxInInfo] = {
        inputs.filter(_.resolved.address === addr)
    }

    def getAdaFromOutputs(outputs: List[TxOut]): Lovelace = {
        outputs.map(_.value.getLovelace).foldLeft(BigInt(0))(_ + _)
    }

    def getAdaFromInputs(inputs: List[TxInInfo]): Lovelace = {
        inputs.map(_.resolved.value.getLovelace).foldLeft(BigInt(0))(_ + _)
    }

    def mustBeSignedBy(signatories: List[PubKeyHash], vkh: PubKeyHash): Boolean = {
        signatories.contains(vkh)
    }

}

@Compile
object Escrow extends Validator:
    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit = {
        log("Escrow script started")
        val receivedData = datum.getOrFail("Datum not found")
        val escrowDatum: EscrowDatum = receivedData.to[EscrowDatum]
        val EscrowRedeemer(action) = redeemer.to[EscrowRedeemer]

        val ownInput = txInfo.inputs
            .find(input => input.outRef === txOutRef)
            .get
            .resolved
        val contractAddress = ownInput.address
        val contractInputs = EscrowUtils.getInputsByAddress(txInfo.inputs, contractAddress)
        val contractBalance = EscrowUtils.getAdaFromInputs(contractInputs)

        log("Contract balance calculation:")
        log("Number of contract inputs:")
        log(contractInputs.length.show)
        log("Contract balance from inputs:")
        log(contractBalance.show)
        log("Own input balance:")
        log(ownInput.value.getLovelace.show)
        log("Expected total (escrow + init):")
        log((escrowDatum.escrowAmount + escrowDatum.initializationAmount).show)

        action match {
            case EscrowAction.Deposit =>
                handleDeposit(escrowDatum, txInfo, contractAddress, contractBalance, receivedData)
            case EscrowAction.Pay =>
                handlePay(escrowDatum, txInfo, contractBalance)
            case EscrowAction.Refund =>
                handleRefund(escrowDatum, txInfo, contractBalance)
        }
    }

    private def handleDeposit(
        escrowDatum: EscrowDatum,
        txInfo: TxInfo,
        contractAddress: Address,
        contractBalance: Lovelace,
        receivedData: Data
    ): Unit = {
        log("Handling deposit action")

        require(
          EscrowUtils.mustBeSignedBy(txInfo.signatories, escrowDatum.buyer),
          "Buyer must sign deposit transaction"
        )

        val buyerOutputs = EscrowUtils.getOutputsByVkh(txInfo.outputs, escrowDatum.buyer)
        val contractOutputs = EscrowUtils.getOutputsByAddress(txInfo.outputs, contractAddress)

        require(contractOutputs.length === BigInt(1), "Expected exactly one contract output")
        val contractOutput = contractOutputs.head

        require(buyerOutputs.length === BigInt(1), "Expected exactly one buyer output")

        require(
          contractBalance != escrowDatum.escrowAmount,
          "Contract must contain only initialization amount before deposit"
        )

        require(
          EscrowUtils.getAdaFromOutputs(
            contractOutputs
          ) === escrowDatum.escrowAmount + escrowDatum.initializationAmount,
          "Contract output must contain exactly escrow amount plus initialization amount"
        )

        contractOutput.datum match {
            case OutputDatum.OutputDatum(inlineData) =>
                require(
                  inlineData === receivedData,
                  "EscrowDatum must be preserved"
                )
            case _ => fail("Expected inline datum")
        }
    }

    private def handlePay(
        escrowDatum: EscrowDatum,
        txInfo: TxInfo,
        contractBalance: Lovelace
    ): Unit = {
        log("Handling pay action")

        require(
          contractBalance === escrowDatum.escrowAmount + escrowDatum.initializationAmount,
          "Contract must be fully funded before payment"
        )

        val buyerOutputs = EscrowUtils.getOutputsByVkh(txInfo.outputs, escrowDatum.buyer)
        val sellerOutputs = EscrowUtils.getOutputsByVkh(txInfo.outputs, escrowDatum.seller)

        require(
          sellerOutputs.nonEmpty,
          "Seller outputs must not be empty"
        )

        require(
          buyerOutputs.nonEmpty,
          "Buyer outputs must not be empty"
        )

        require(
          EscrowUtils.mustBeSignedBy(txInfo.signatories, escrowDatum.buyer),
          "Only buyer can release payment"
        )

        require(
          EscrowUtils.getAdaFromOutputs(
            sellerOutputs
          ) === escrowDatum.escrowAmount + escrowDatum.initializationAmount,
          "Seller must receive exactly escrow amount plus initialization amount"
        )
    }

    private def handleRefund(
        escrowDatum: EscrowDatum,
        txInfo: TxInfo,
        contractBalance: Lovelace
    ): Unit = {
        log("Handling refund action")

        require(
          contractBalance === escrowDatum.escrowAmount + escrowDatum.initializationAmount,
          "Contract must be fully funded before refund"
        )

        val buyerOutputs = EscrowUtils.getOutputsByVkh(txInfo.outputs, escrowDatum.buyer)
        val sellerOutputs = EscrowUtils.getOutputsByVkh(txInfo.outputs, escrowDatum.seller)

        require(
          sellerOutputs.nonEmpty,
          "Seller outputs must not be empty"
        )

        require(
          buyerOutputs.nonEmpty,
          "Buyer outputs must not be empty"
        )

        require(
          EscrowUtils.mustBeSignedBy(txInfo.signatories, escrowDatum.seller),
          "Only seller can issue refund"
        )

        require(
          EscrowUtils.getAdaFromOutputs(buyerOutputs) === escrowDatum.escrowAmount,
          "Buyer must receive exactly the escrow amount back"
        )
    }

object EscrowScript {
    inline def compiled(using scalus.Compiler.Options) = compile(Escrow.validate)
    inline def doubleCborHex(using scalus.Compiler.Options) =
        compiled.toUplc(true).plutusV3.doubleCborHex
}
