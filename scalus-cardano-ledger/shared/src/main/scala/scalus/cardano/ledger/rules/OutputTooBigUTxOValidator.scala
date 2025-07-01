package scalus.cardano.ledger
package rules

import io.bullet.borer.Cbor
import scala.util.boundary
import scala.util.boundary.break

// It's Babbage.validateOutputTooBigUTxO in cardano-ledger
object OutputTooBigUTxOValidator extends STS.Validator {
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
          context.env.params.maxValueSize,
          (transactionId, maxValueSize, outputValueSerializationSize, index) =>
              IllegalArgumentException(
                s"Transaction output at index $index in transaction $transactionId is too big: " +
                    s"expected at most $maxValueSize, got $outputValueSerializationSize bytes"
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
          context.env.params.maxValueSize,
          (transactionId, maxValueSize, outputValueSerializationSize, _) =>
              IllegalArgumentException(
                s"Collateral return output in transaction $transactionId is too big: " +
                    s"expected at most $maxValueSize, got $outputValueSerializationSize bytes"
              )
        )
    }

    private def validateOutputs(
        outputs: scala.collection.View[(Sized[TransactionOutput], Int)],
        transactionId: TransactionHash,
        maxValueSize: Long,
        invalidOutputError: (
            TransactionHash,
            Long,
            Long,
            Int
        ) => IllegalArgumentException
    ): Result = boundary {
        for (Sized(output, _), index) <- outputs
        do
            // TODO maybe make serialization depending on the protocol version
            // serSize = fromIntegral $ BSL.length $ serialize (pvMajor protVer) v
            val outputValueSerializationSize = Cbor.encode(output.value).toByteArray.length

            if outputValueSerializationSize > maxValueSize then
                break(
                  failure(
                    invalidOutputError(
                      transactionId,
                      maxValueSize,
                      outputValueSerializationSize,
                      index
                    )
                  )
                )

        success
    }
}
