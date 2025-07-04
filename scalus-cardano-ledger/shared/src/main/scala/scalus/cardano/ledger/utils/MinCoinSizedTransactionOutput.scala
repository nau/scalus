package scalus.cardano.ledger
package utils

import scalus.ledger.babbage.ProtocolParams

object MinCoinSizedTransactionOutput {
    def apply(
        protocolParams: ProtocolParams,
        sizedTransactionOutput: Sized[TransactionOutput]
    ): Coin = {
        val utxoCostPerByte = protocolParams.utxoCostPerByte
        val transactionOutputSize = sizedTransactionOutput.size

        Coin((constantOverhead + transactionOutputSize) * utxoCostPerByte)
    }

    private val constantOverhead = 160
}
