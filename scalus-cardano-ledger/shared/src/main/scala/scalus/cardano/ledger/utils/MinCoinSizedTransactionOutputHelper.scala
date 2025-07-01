package scalus.cardano.ledger.utils

import scalus.ledger.babbage.ProtocolParams
import scalus.cardano.ledger.{Coin, Sized, TransactionOutput}

object MinCoinSizedTransactionOutputHelper {
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
