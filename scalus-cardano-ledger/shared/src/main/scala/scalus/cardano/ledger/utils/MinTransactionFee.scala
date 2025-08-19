package scalus.cardano.ledger
package utils

import io.bullet.borer.Cbor
import scalus.ledger.babbage.ProtocolParams

import scala.annotation.tailrec

object MinTransactionFee {
    def apply(
        transaction: Transaction,
        utxo: UTxO,
        protocolParams: ProtocolParams
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Coin
    ] = {
        for scripts <- AllProvidedReferenceScripts.allProvidedReferenceScripts(transaction, utxo)
        yield
            val refScriptsFee = RefScriptsFeeCalculator.calculateFee(scripts, protocolParams)
            val transactionSizeFee = calculateTransactionSizeFee(transaction, protocolParams)
            val exUnitsFee = calculateExUnitsFee(transaction, protocolParams)

            refScriptsFee + transactionSizeFee + exUnitsFee
    }

    /** Calculates the transaction size fee based on the protocol parameters.
      *
      * @param transaction
      *   The transaction for which the fee is calculated.
      * @param protocolParams
      *   The protocol parameters containing fee-related settings.
      * @return
      *   The calculated fee as a Coin.
      */
    def calculateTransactionSizeFee(
        transaction: Transaction,
        protocolParams: ProtocolParams
    ): Coin = {
        val txFeeFixed = protocolParams.txFeeFixed
        val txFeePerByte = protocolParams.txFeePerByte
        val transactionSize = Cbor.encode(transaction).toByteArray.length

        Coin(transactionSize * txFeePerByte + txFeeFixed)
    }

    private def calculateExUnitsFee(
        transaction: Transaction,
        protocolParams: ProtocolParams
    ): Coin = {
        val executionUnitPrices = protocolParams.executionUnitPrices
        val totalExUnits = calculateTotalExUnits(transaction)

        if totalExUnits == ExUnits.zero then Coin.zero
        else
            Coin(
              (
                executionUnitPrices.priceMemory * totalExUnits.memory +
                    executionUnitPrices.priceSteps * totalExUnits.steps
              ).ceil
            )
    }

    private def calculateTotalExUnits(transaction: Transaction): ExUnits = {
        transaction.witnessSet.redeemers
            .map(_.value.toSeq.foldLeft(ExUnits.zero) { (exUnits, redeemer) =>
                exUnits + redeemer.exUnits
            })
            .getOrElse(ExUnits.zero)
    }
}

object RefScriptsFeeCalculator {

    /** Calculates the fee for reference scripts based on their size and the protocol parameters.
      *
      * @param scripts
      *   The set of scripts to calculate the fee for.
      * @param protocolParams
      *   The protocol parameters containing fee-related settings.
      * @return
      *   The calculated fee as a Coin.
      */
    def calculateFee(scripts: Set[Script], protocolParams: ProtocolParams): Coin = {
        val refScriptsSize = scripts.foldLeft(0) { case (length, script) =>
            val scripLength = script match
                case _: Script.Native        => 0 // Native scripts do not contribute to fees
                case Script.PlutusV1(script) => script.size
                case Script.PlutusV2(script) => script.size
                case Script.PlutusV3(script) => script.size

            length + scripLength
        }
        calculateFee(refScriptsSize, protocolParams)
    }

    /** Calculates the fee for reference scripts based on their size and the protocol parameters.
      *
      * @param refScriptsSize
      *   The total size of all reference scripts in bytes.
      * @param protocolParams
      *   The protocol parameters containing fee-related settings.
      * @return
      *   The calculated fee as a Coin.
      */
    def calculateFee(refScriptsSize: Int, protocolParams: ProtocolParams): Coin = {
        def tierRefScriptFee(
            multiplier: NonNegativeInterval,
            sizeIncrement: Int,
            curTierPrice: NonNegativeInterval,
            n: Int
        ): Coin = {
            @tailrec
            def go(
                acc: NonNegativeInterval,
                curTierPrice: NonNegativeInterval,
                n: Int
            ): Coin = {
                if n < sizeIncrement then Coin((acc + curTierPrice * n).floor)
                else
                    go(
                      acc + curTierPrice * sizeIncrement,
                      multiplier * curTierPrice,
                      n - sizeIncrement
                    )
            }

            go(NonNegativeInterval.zero, curTierPrice, n)
        }

        val minFeeRefScriptCostPerByte = NonNegativeInterval(
          protocolParams.minFeeRefScriptCostPerByte
        )

        tierRefScriptFee(
          refScriptCostMultiplier,
          refScriptCostStride,
          minFeeRefScriptCostPerByte,
          refScriptsSize
        )
    }

    private val refScriptCostMultiplier = NonNegativeInterval(1.2)
    private val refScriptCostStride = 25600
}
