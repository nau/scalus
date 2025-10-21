package scalus.cardano.txbuilder

case class BuilderContext(
    env: Environment,
    wallet: Wallet
) {

    def buildNewTx: TxBuilder = TxBuilder(this)
}
