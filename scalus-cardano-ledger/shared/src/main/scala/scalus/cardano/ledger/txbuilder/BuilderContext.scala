package scalus.cardano.ledger.txbuilder
import com.bloxbean.cardano.client.api.UtxoSupplier
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.function.TxSigner as CCLSigner
import scalus.builtin.ByteString
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.rules.{Context, State, UtxoEnv}
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.{CertState, Coin, PlutusScriptEvaluator, Sized, Transaction, TransactionException, TransactionInput, TransactionOutput, Utxos, Value}
import scalus.cardano.ledger.ProtocolParams

case class BuilderContext(
    env: Environment,
    wallet: Wallet
) {

    def buildNewTx = TxBuilder(this)
}
