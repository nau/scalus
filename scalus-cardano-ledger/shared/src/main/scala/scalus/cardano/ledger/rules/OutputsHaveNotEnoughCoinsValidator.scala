package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.utils.MinCoinSizedTransactionOutput

// It's Babbage.validateOutputTooSmallUTxO in cardano-ledger
object OutputsHaveNotEnoughCoinsValidator extends STS.Validator {
    override final type Error = TransactionException.OutputsHaveNotEnoughCoinsException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionId = event.id
        val protocolParams = context.env.params
        val outputs = event.body.value.outputs
        val collateralReturnOutput = event.body.value.collateralReturnOutput

        val invalidOutputs = findInvalidOutputs(outputs, protocolParams)

        val invalidCollateralReturnOutput = findInvalidOutputs(
          collateralReturnOutput.map(IndexedSeq.apply(_)).getOrElse(IndexedSeq.empty),
          protocolParams
        ).headOption

        if invalidOutputs.nonEmpty || invalidCollateralReturnOutput.nonEmpty then
            return failure(
              TransactionException.OutputsHaveNotEnoughCoinsException(
                transactionId,
                invalidOutputs,
                invalidCollateralReturnOutput
              )
            )

        success
    }

    private def findInvalidOutputs(
        outputs: IndexedSeq[Sized[TransactionOutput]],
        protocolParams: ProtocolParams
    ): IndexedSeq[(TransactionOutput, Coin)] = {
        for
            sizedOutput @ Sized(output, _) <- outputs
            minCoinSizedTransactionOutput = MinCoinSizedTransactionOutput(
              sizedOutput,
              protocolParams
            )
            if output.value.coin < minCoinSizedTransactionOutput
        yield (output, minCoinSizedTransactionOutput)
    }
}
