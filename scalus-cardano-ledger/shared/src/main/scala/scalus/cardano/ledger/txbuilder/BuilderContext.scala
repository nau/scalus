package scalus.cardano.ledger.txbuilder
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.{Context, State, UtxoEnv}

case class BuilderContext(
    env: Environment,
    wallet: Wallet
) {

    def buildNewTx: TxBuilder = TxBuilder(this)
}
