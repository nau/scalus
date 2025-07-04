package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.utils.MinCoinSizedTransactionOutput
import scalus.ledger.babbage.ProtocolParams
import scala.util.boundary
import scala.util.boundary.break

// It's Babbage.validateOutputTooSmallUTxO in cardano-ledger
object OutputTooSmallUTxOValidator extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = {
        for
            _ <- validateTransactionOutputs(context, state, event)
            _ <- validateCollateralReturnOutput(context, state, event)
        yield ()
    }

    private def validateTransactionOutputs(context: Context, state: State, event: Event): Result = {
        validateOutputs(
          event.body.value.outputs.view.zipWithIndex,
          event.id,
          context.env.params,
          (transactionId, minCoinSizedTransactionOutput, output, index) =>
              IllegalArgumentException(
                s"Transaction output at index $index in transaction $transactionId is too small: " +
                    s"expected at least $minCoinSizedTransactionOutput, got ${output.value.coin}"
              )
        )
    }

    private def validateCollateralReturnOutput(
        context: Context,
        state: State,
        event: Event
    ): Result = {
        validateOutputs(
          event.body.value.collateralReturnOutput.view.zipWithIndex,
          event.id,
          context.env.params,
          (transactionId, minCoinSizedCollateralReturnOutput, collateralReturnOutput, _) =>
              IllegalArgumentException(
                s"Collateral return output in transaction $transactionId is too small: " +
                    s"expected at least $minCoinSizedCollateralReturnOutput, got ${collateralReturnOutput.value.coin}"
              )
        )
    }

    private def validateOutputs(
        outputs: scala.collection.View[(Sized[TransactionOutput], Int)],
        transactionId: TransactionHash,
        protocolParams: ProtocolParams,
        invalidOutputError: (
            TransactionHash,
            Coin,
            TransactionOutput,
            Int
        ) => IllegalArgumentException
    ): Result = boundary {
        for (sizedOutput @ Sized(output, _), index) <- outputs
        do
            val minCoinSizedTransactionOutput =
                MinCoinSizedTransactionOutput(protocolParams, sizedOutput)

            if output.value.coin < minCoinSizedTransactionOutput then
                break(
                  failure(
                    invalidOutputError(transactionId, minCoinSizedTransactionOutput, output, index)
                  )
                )

        success
    }
}
