package scalus.cardano.ledger.txbuilder

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.quicktx.{QuickTxBuilder, Tx}
import scalus.cardano.ledger.*

case class RegisterDRepTransactionBuilder(
    context: BuilderContext,
    account: Account
) {

    def buildAndSign(): Transaction = {
        val cclTx = QuickTxBuilder(context.backendService)
            .compose(
              new Tx()
                  .registerDRep(account)
                  .from(account.baseAddress())
                  .withChangeAddress(account.baseAddress())
            )
            .withSigner(SignerProviders.signerFrom(account))
            .withSigner(SignerProviders.signerFrom(account.drepHdKeyPair()))
            .buildAndSign()

        Transaction.fromCbor(cclTx.serialize())
    }
}
