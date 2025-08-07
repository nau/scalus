package scalus.cardano.ledger.txbuilder
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.{Coin, PlutusScriptEvaluator, Sized, Transaction, TransactionOutput, UTxO, Value}
import scalus.ledger.babbage.ProtocolParams

case class BuilderContext(
    protocolParams: ProtocolParams,
    evaluator: PlutusScriptEvaluator,
    network: Network,
    utxoProvider: UtxoProvider,
    onSurplus: OnSurplus,
) {

    def buildNewTx(validators: Seq[Validator] = Seq.empty): TxBuilder = TxBuilder(this)
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
