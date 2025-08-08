package scalus.cardano.ledger.txbuilder
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.rules.{Context, State, UtxoEnv}
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.{CertState, Coin, PlutusScriptEvaluator, Sized, Transaction, TransactionException, TransactionInput, TransactionOutput, UTxO, Value}
import scalus.ledger.babbage.ProtocolParams

case class BuilderContext(
    protocolParams: ProtocolParams,
    evaluator: PlutusScriptEvaluator,
    network: Network,
    utxoProvider: UtxoProvider,
    onSurplus: OnSurplus,
    validators: Seq[Validator] = Seq.empty
) {
    def buildNewTx: TxBuilder = TxBuilder(this)
    def utxo = utxoProvider.utxo

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
}
object UtxoProvider {
    def from(u: UTxO): UtxoProvider = new UtxoProvider { override def utxo: UTxO = u }
}

trait SelectInputs {
    def selectInputs(utxo: UTxO): Set[TransactionInput]
    final def apply(utxo: UTxO): Set[TransactionInput] = selectInputs(utxo)
}
object SelectInputs {
    def all: SelectInputs = (utxo: UTxO) => utxo.keySet
    def particular(inputs: Set[TransactionInput]): SelectInputs = _ => inputs
}
