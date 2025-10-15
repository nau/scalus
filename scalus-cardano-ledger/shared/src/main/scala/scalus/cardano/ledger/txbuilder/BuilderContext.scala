package scalus.cardano.ledger.txbuilder

case class BuilderContext(
    env: Environment,
    wallet: Wallet
) {

    def buildNewTx: TxBuilder = TxBuilder(this)
}
