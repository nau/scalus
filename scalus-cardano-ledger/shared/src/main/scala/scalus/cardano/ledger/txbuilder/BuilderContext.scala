package scalus.cardano.ledger.txbuilder
import com.bloxbean.cardano.client.api.UtxoSupplier
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.function.TxSigner as CCLSigner
import scalus.builtin.ByteString
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.rules.{Context, State, UtxoEnv}
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.{CertState, Coin, PlutusScriptEvaluator, Sized, Transaction, TransactionException, TransactionInput, TransactionOutput, UTxO, Value}
import scalus.ledger.babbage.ProtocolParams

/** A context for transaction builders.
  *
  * Allows to broadly configure transaction builders with protocol params, script evaluators, etc.
  * Builders produced by this context allow more fine-grained transaction modification.
  */
case class BuilderContext(
    protocolParams: ProtocolParams,
    evaluator: PlutusScriptEvaluator,
    network: Network,
    utxoProvider: UtxoProvider,
    validators: Seq[Validator] = Seq.empty,
    backendService: BackendService = null,
) {

    /** Initializes a new transaction builder using this context. */
    def buildNewTx: TxBuilder = TxBuilder(this, backendService = backendService)

    def utxo: UTxO = utxoProvider.utxo

    /** Modifies this context to use the specified utxo. The existing utxo will be extended with the
      * specified one.
      */
    def withUtxo(utxo: UTxO): BuilderContext = copy(utxoProvider = utxoProvider.extendWith(utxo))


    /** Validates the transaction against the [[validators]] of this context. */
    def validate(tx: Transaction): Either[TransactionException, Transaction] = {
        val certState = CertState.empty
        val context = Context(tx.body.value.fee, UtxoEnv(1L, protocolParams, certState))
        val state = State(utxo, certState)
        validators
            .map(_.validate(context, state, tx))
            .collectFirst { case l: Left[?, ?] => l.value }
            .toLeft(tx)
    }
}

/** A behavior that takes place when a payment transaction produces more ada than it consumes. */
trait OnSurplus {
    def apply(utxo: UTxO, surplus: Coin): Transaction => Transaction
}
object OnSurplus {
    import TxBuilder.modifyBody
    def toFee: OnSurplus = (_, surplus: Coin) =>
        tx => {
            val fee = tx.body.value.fee + surplus
            modifyBody(tx, _.copy(fee = fee))
        }

    def toAddress(address: Address): OnSurplus = (_, surplus: Coin) =>
        tx => {
            val changeOutput = TransactionOutput(address, Value(surplus))
            modifyBody(tx, b => b.copy(outputs = b.outputs :+ Sized(changeOutput)))

        }

    def toFirstPayer: OnSurplus = (utxo: UTxO, surplus: Coin) =>
        tx => {
            val firstPayer = utxo.head
            toAddress(firstPayer._2.address)(utxo, surplus)(tx)
        }

    def donate: OnSurplus = (_, surplus: Coin) =>
        tx => modifyBody(tx, _.copy(donation = Some(surplus)))
}

trait UtxoProvider {
    def utxo: UTxO
    def extendWith(utxo: UTxO): UtxoProvider
}
object UtxoProvider {
    private case class InMemoryUtxo(u: UTxO) extends UtxoProvider {
        override def utxo: UTxO = u
        override def extendWith(u2: UTxO): UtxoProvider = copy(u = u ++ u2)
    }

    def from(u: UTxO): UtxoProvider = InMemoryUtxo(u)
}

trait SelectInputs {
    def selectInputs(utxo: UTxO): Set[TransactionInput]
    final def apply(utxo: UTxO): Set[TransactionInput] = selectInputs(utxo)
}
object SelectInputs {
    def all: SelectInputs = (utxo: UTxO) => utxo.keySet
    def particular(inputs: Set[TransactionInput]): SelectInputs = _ => inputs
}
