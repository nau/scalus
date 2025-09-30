package scalus.cardano.ledger
package utils

object MinCoinSizedTransactionOutput {
    def apply(
        sizedTransactionOutput: Sized[TransactionOutput],
        protocolParams: ProtocolParams
    ): Coin = {
        val utxoCostPerByte = protocolParams.utxoCostPerByte
        val transactionOutputSize = sizedTransactionOutput.size

        Coin((constantOverhead + transactionOutputSize) * utxoCostPerByte)
    }

    private val constantOverhead = 160
}
