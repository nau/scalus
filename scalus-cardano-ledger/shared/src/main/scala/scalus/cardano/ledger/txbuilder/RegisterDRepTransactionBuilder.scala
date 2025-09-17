package scalus.cardano.ledger.txbuilder

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.quicktx.{QuickTxBuilder, Tx}
import scalus.cardano.address.Address
import scalus.cardano.ledger.*

case class RegisterDRepTransactionBuilder(
    context: BuilderContext,
    account: Account
) {

    def buildAndSign(): Transaction = {
        val cclTx = QuickTxBuilder(context.backendService)
            .compose(new Tx().registerDRep(account).from(account.baseAddress()))
            .withSigner(SignerProviders.signerFrom(account))
            .withSigner(SignerProviders.signerFrom(account.drepHdKeyPair()))
            .buildAndSign()

        Transaction.fromCbor(cclTx.serialize())
    }
}

extension (txBuilder: TxBuilder) {
    def registerDRep(credential: Credential, deposit: Coin, anchor: Option[Anchor]): TxBuilder = {
        val certificate = Certificate.RegDRepCert(credential, deposit, anchor)
        val updatedCertificates =
            TaggedSet.from(txBuilder.tx.body.value.certificates.toIndexedSeq :+ certificate)
        txBuilder.copy(tx =
            TxBuilder.modifyBody(txBuilder.tx, b => b.copy(certificates = updatedCertificates))
        )
    }
}
