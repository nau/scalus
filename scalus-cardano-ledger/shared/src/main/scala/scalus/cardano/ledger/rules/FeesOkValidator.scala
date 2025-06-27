package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.rules.utils.AllReferenceScripts
import io.bullet.borer.Cbor
import scalus.cardano.ledger.Script.PlutusV1
import scala.util.boundary
import scala.util.boundary.break
import scala.annotation.tailrec

// It's Babbage.FeesOK in cardano-ledger
//feesOK is a predicate with several parts. Some parts only apply in special circumstances.
//--   1) The fee paid is >= the minimum fee
//--   2) If the total ExUnits are 0 in both Memory and Steps, no further part needs to be checked.
//--   3) The collateral consists only of VKey addresses
//--   4) The collateral inputs do not contain any non-ADA part
//--   5) The collateral is sufficient to cover the appropriate percentage of the
//--      fee marked in the transaction
//--   6) The collateral is equivalent to total collateral asserted by the transaction
//--   7) There is at least one collateral input
object FeesOkValidator extends STS.Validator, AllReferenceScripts {
    override def validate(context: Context, state: State, event: Event): Result = boundary {
        val transactionId = event.id
        val collateralInputs = event.body.value.collateralInputs
        val utxo = state.utxo

        for
            _ <- feePaidIsGreeterOrEqualThanMinimumFee(context, state, event)
            _ <-
                if totalExUnitsAreZero(event) then success
                else
                    val collateralCoins =
                        for
                            (collateralInput, index) <- collateralInputs.view.zipWithIndex
                            collateralCoin =
                                val validatedCollateralCoin =
                                    for
                                        collateralOutput <- extractCollateralOutput(
                                          transactionId,
                                          collateralInput,
                                          utxo,
                                          index
                                        )
                                        _ <- collateralConsistsOnlyOfVKeyAddress(
                                          transactionId,
                                          collateralInput,
                                          collateralOutput,
                                          index
                                        )
                                        _ <- collateralDoesNotContainAnyNonADA(
                                          transactionId,
                                          collateralInput,
                                          collateralOutput,
                                          index
                                        )
                                    yield collateralOutput.value.coin

                                validatedCollateralCoin match
                                    case Right(coin) => coin
                                    case Left(error) => break(failure(error))
                        yield collateralCoin

                    val totalSumOfCollateralCoins = collateralCoins.foldLeft(Coin.zero)(_ + _)

                    for
                        _ <- totalSumOfCollateralCoinsIsSufficient(
                          context,
                          event,
                          totalSumOfCollateralCoins
                        )
                        _ <- totalSumOfCollateralCoinsIsEquivalentToTotalCollateral(
                          event,
                          totalSumOfCollateralCoins
                        )
                        _ <- isAtLeastOneCollateralInput(event)
                    yield ()
        yield ()
    }

    //
    private def feePaidIsGreeterOrEqualThanMinimumFee(
        context: Context,
        state: State,
        event: Event
    ): Result = {
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

    private def totalExUnitsAreZero(event: Event): Boolean = {
        // The total ExUnits are zero if there are no redeemers in the witness set
        // or if all redeemers have ExUnits.zero.
        // This is a simplified check, as in original haskell cardano code and specification.
//        calculateTotalExUnits(event: Event) == ExUnits.zero
        event.witnessSet.redeemers.isEmpty
    }

    private def extractCollateralOutput(
        transactionId: TransactionHash,
        collateralInput: TransactionInput,
        utxo: Utxo,
        index: Int
    ): Either[Error, TransactionOutput] = {
        utxo.get(collateralInput) match
            case Some(collateralOutput) => Right(collateralOutput)
            // This check allows to be an order independent in the sequence of validation rules
            case None =>
                Left(
                  IllegalArgumentException(
                    s"Collateral input $collateralInput at index $index is missing in UTXO for transactionId $transactionId"
                  )
                )
    }

    private def collateralConsistsOnlyOfVKeyAddress(
        transactionId: TransactionHash,
        collateralInput: TransactionInput,
        collateralOutput: TransactionOutput,
        index: Int
    ): Result = {
        if collateralOutput.address.keyHash.isEmpty then
            failure(
              IllegalArgumentException(
                s"Collateral input $collateralInput at index $index is not a VKey address in UTXO for transactionId $transactionId"
              )
            )
        else success
    }

    private def collateralDoesNotContainAnyNonADA(
        transactionId: TransactionHash,
        collateralInput: TransactionInput,
        collateralOutput: TransactionOutput,
        index: Int
    ): Result = {
        if collateralOutput.value.assets.nonEmpty then
            failure(
              IllegalArgumentException(
                s"Collateral input $collateralInput at index $index contains non-ADA assets in UTXO for transactionId $transactionId"
              )
            )
        else success
    }

    private def totalSumOfCollateralCoinsIsSufficient(
        context: Context,
        event: Event,
        totalSumOfCollateralCoins: Coin
    ): Result = {
        val transactionId = event.id
        val transactionFee = event.body.value.fee.value
        val collateralReturnOutput = event.body.value.collateralReturnOutput
        val collateralPercentage = context.env.params.collateralPercentage

        val deltaCoins = collateralReturnOutput match
            case Some(collateralReturnOutput) =>
                totalSumOfCollateralCoins.value - collateralReturnOutput.value.coin.value
            case None => totalSumOfCollateralCoins.value

        if (deltaCoins * 100) < transactionFee * collateralPercentage then
            failure(
              IllegalArgumentException(
                s"Total sum of collateral coins $totalSumOfCollateralCoins are insufficient for transaction fee $transactionFee with collateral percentage $collateralPercentage% and collateral return output $collateralReturnOutput for transactionId $transactionId"
              )
            )
        else success
    }

    private def totalSumOfCollateralCoinsIsEquivalentToTotalCollateral(
        event: Event,
        totalSumOfCollateralCoins: Coin
    ): Result = {
        val transactionId = event.id
        val totalCollateral = event.body.value.totalCollateral

        totalCollateral match
            case None => success
            case Some(collateral) =>
                if collateral.value != totalSumOfCollateralCoins.value then
                    failure(
                      IllegalArgumentException(
                        s"Total collateral $collateral is not equivalent to total sum of collateral coins $totalSumOfCollateralCoins for transactionId $transactionId"
                      )
                    )
                else success
    }

    private def isAtLeastOneCollateralInput(
        event: Event
    ): Result = {
        if event.body.value.collateralInputs.isEmpty then
            failure(
              IllegalArgumentException(
                s"There is no collateral input in transaction ${event.id}"
              )
            )
        else success
    }

    private def calculateMinTransactionFee(
        context: Context,
        state: State,
        event: Event
    ): Either[Error, Coin] = {
        for scripts <- allReferenceScripts(state, event)
        yield
            val refScriptsFee = RefScriptsFeeCalculator(context, scripts)
            val transactionSizeFee = calculateTransactionSizeFee(context, event)
            val exUnitsFee = calculateExUnitsFee(context, event)

            refScriptsFee + transactionSizeFee + exUnitsFee
    }

    private object RefScriptsFeeCalculator {
        def apply(context: Context, scripts: Set[Script]): Coin = {
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

            val refScriptsSize = scripts.foldLeft(0) { case (length, script) =>
                val scripLength = script match
                    case _: Script.Native => 0 // Native scripts do not contribute to fees
                    case Script.PlutusV1(plutusV1Script) => plutusV1Script.byteString.size
                    case Script.PlutusV2(plutusV2Script) => plutusV2Script.byteString.size
                    case Script.PlutusV3(plutusV3Script) => plutusV3Script.byteString.size

                length + scripLength
            }

            val minFeeRefScriptCostPerByte = NonNegativeInterval(
              context.env.params.minFeeRefScriptCostPerByte
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

    private def calculateTransactionSizeFee(context: Context, event: Event): Coin = {
        val txFeeFixed = context.env.params.txFeeFixed
        val txFeePerByte = context.env.params.txFeePerByte
        val transactionSize = Cbor.encode(event).toByteArray.length

        Coin(transactionSize * txFeePerByte + txFeeFixed)
    }

    private def calculateExUnitsFee(context: Context, event: Event): Coin = {
        val executionUnitPrices = context.env.params.executionUnitPrices
        val totalExUnits = calculateTotalExUnits(event)

        if totalExUnits == ExUnits.zero then Coin.zero
        else
            Coin(
              (
                executionUnitPrices.priceMemory * totalExUnits.memory +
                    executionUnitPrices.priceSteps * totalExUnits.steps
              ).ceil
            )
    }

    private def calculateTotalExUnits(event: Event): ExUnits = {
        event.witnessSet.redeemers
            .map(_.value.toSeq.foldLeft(ExUnits.zero) { (exUnits, redeemer) =>
                exUnits + redeemer.exUnits
            })
            .getOrElse(ExUnits.zero)
    }
}
