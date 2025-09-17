package scalus.cardano.ledger.txbuilder

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.quicktx.{QuickTxBuilder, Tx}
import scalus.cardano.address.Address
import scalus.cardano.ledger.{Anchor, Transaction}

case class RegisterDRepTransactionBuilder(
    txBuilder: TxBuilder,
    address: Address,
    var anchor: Anchor = null,
    var account: Account = null
) {

    def withAccount(a: Account) = {
        this.account = a
        this
    }

    def withAnchor(a: Anchor) = {
        this.anchor = a
        this
    }

    def buildAndSign(signer: TxSigner) = {}

    def build = {
        val cclTx = new QuickTxBuilder(txBuilder.backendService)
            .compose(
              new Tx()
                  .registerDRep(account)
                  .from(address.encode.get)
            )
            .validTo(Long.MaxValue)
            .validFrom(0)
            .withSigner(t => t)
            .build()

        Transaction.fromCbor(cclTx.serialize())
    }
}
