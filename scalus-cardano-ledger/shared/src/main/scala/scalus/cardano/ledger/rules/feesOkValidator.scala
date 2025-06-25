package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.rules.utils.AllReferenceScripts
import io.bullet.borer.Cbor
import scalus.cardano.ledger.Script.PlutusV1

import scala.annotation.tailrec

// It's Babbage.FeesOK in cardano-ledger
object feesOkValidator extends STS.Validator, AllReferenceScripts {
    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionFee = event.body.value.fee
        for
            minTransactionFee <- calculateMinTransactionFee(context, state, event)
            _ <-
                if transactionFee < minTransactionFee then
                    failure(
                      IllegalArgumentException(
                        s"Transaction fee $transactionFee is less than minimum transaction fee $minTransactionFee"
                      )
                    )
                else success
        yield ()
    }

    private def calculateMinTransactionFee(
        context: Context,
        state: State,
        event: Event
    ): Either[Error, Coin] = {
        def tierRefScriptFee(
            multiplier: NonNegativeInterval,
            sizeIncrement: Int,
            curTierPrice: NonNegativeInterval,
            n: Int
        ): Coin = {
            @tailrec
            def go(acc: NonNegativeInterval, curTierPrice: NonNegativeInterval, n: Int): Coin = {
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

        for scripts <- allReferenceScripts(state, event)
        yield {
            val refScriptsSize = scripts.foldLeft(0) { case (length, script) =>
                val scripLength = script match
                    case _: Script.Native => 0 // Native scripts do not contribute to fees
                    case Script.PlutusV1(plutusV1Script) => plutusV1Script.byteString.bytes.length
                    case Script.PlutusV2(plutusV2Script) => plutusV2Script.byteString.bytes.length
                    case Script.PlutusV3(plutusV3Script) => plutusV3Script.byteString.bytes.length

                length + scripLength
            }

            val minFeeRefScriptCostPerByte = NonNegativeInterval(
              context.env.params.minFeeRefScriptCostPerByte
            )

            val refScriptsFee = tierRefScriptFee(
              refScriptCostMultiplier,
              refScriptCostStride,
              minFeeRefScriptCostPerByte,
              refScriptsSize
            )

            val txFeeFixed = context.env.params.txFeeFixed
            val txFeePerByte = context.env.params.txFeePerByte
            val transactionSizeFee = Coin(
              Cbor.encode(event).toByteArray.length * txFeePerByte + txFeeFixed
            )

            val executionUnitPrices = context.env.params.executionUnitPrices
            val exUnits = event.witnessSet.redeemers
                .map(_.toSeq.foldLeft(ExUnits.zero) { (exUnits, redeemer) =>
                    exUnits + redeemer.exUnits
                })
                .getOrElse(ExUnits.zero)
            val exUnitsFee = Coin(
              (executionUnitPrices.priceMemory * exUnits.memory + executionUnitPrices.priceSteps * exUnits.steps).ceil
            )

            refScriptsFee + transactionSizeFee + exUnitsFee
        }
    }

    private val refScriptCostMultiplier = NonNegativeInterval(1.2)
    private val refScriptCostStride = 25600
}
