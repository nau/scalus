package scalus.cardano.ledger.txbuilder
import scalus.builtin.Data
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.utils.TxBalance
import scalus.cardano.ledger.*
import scalus.ledger.babbage.ProtocolParams

val emptyTx: Transaction = Transaction(
  TransactionBody(Set.empty, IndexedSeq.empty, Coin.zero),
  TransactionWitnessSet.empty
)

def realize(intention: Intention)(transaction: Transaction = emptyTx): Transaction =
    intention(transaction)

case class TransactionBuilder(
    body: TransactionBody,
    witnessSet: TransactionWitnessSet,
    auxiliaryData: Option[AuxiliaryData]
)

trait TransactionResolver {
    def resolve(txIn: TransactionInput): Option[TransactionOutput]
}
object TransactionResolver {
    def apply(utxo: UTxO): TransactionResolver = (txIn: TransactionInput) => utxo.get(txIn)
}

trait FeePayerStrategy {
    def apply(
        feeAmount: Coin,
        outputs: IndexedSeq[TransactionOutput]
    ): IndexedSeq[TransactionOutput]
}

object FeePayerStrategy {
    def subtractFromFirstOutput: FeePayerStrategy =
        (feeAmount: Coin, outputs: IndexedSeq[TransactionOutput]) =>
            outputs match {
                case head +: rest => (head - feeAmount) +: rest
                // unsure if we have to `case _ =>`, because a transaction with no outputs will fail down the line anyway
            }
    def subtractFromAddress(address: Address): FeePayerStrategy =
        (feeAmount: Coin, outputs: IndexedSeq[TransactionOutput]) =>
            outputs.map {
                case o if o.address == address => o - feeAmount
                case o                         => o
            }
}

case class ResolvedInputs(state: UTxO) {
    def add(u: (TransactionInput, TransactionOutput)): ResolvedInputs =
        copy(state = state + u)
}

object ResolvedInputs {
    def from(u: (TransactionInput, TransactionOutput)): ResolvedInputs =
        ResolvedInputs(Map(u))
}

trait ChangeReturnStrategy {
    def apply(
        changeAmount: Coin,
        body: TransactionBody,
        utxo: UTxO
    ): IndexedSeq[TransactionOutput]
}

object ChangeReturnStrategy {
    def toAddress(address: Address): ChangeReturnStrategy =
        (changeAmount: Coin, body: TransactionBody, utxo: UTxO) =>
            val outputs = body.outputs.map(_.value)
            if !outputs.exists(_.address == address) then {
                TransactionOutput(address, Value(changeAmount)) +: outputs
            } else {
                outputs.map {
                    case x if x.address == address => x + changeAmount
                    case x                         => x
                }
            }
}

case class Environment(
    protocolParams: ProtocolParams,
    evaluator: PlutusScriptEvaluator,
    network: Network
)

trait Intention {
    def apply(txBody: Transaction): Transaction
}

case class PayTo(
    address: Address,
    amount: Value,
    transactionResolver: TransactionResolver,
    datum: Option[DatumOption] = None,
    environment: Option[Environment] = None,
    resolvedInputs: Option[ResolvedInputs] = None,
    feePayerStrategy: Option[FeePayerStrategy] = None,
    changeReturnStrategy: Option[ChangeReturnStrategy] = None,
) extends Intention {

    override def apply(tx: Transaction): Transaction = (for {
        env <- environment
        inputs <- resolvedInputs
        feePayer <- feePayerStrategy
        changeReturn <- changeReturnStrategy
    } yield {
        val input = inputs.state.keySet
        val out = TransactionOutput(address, amount, datum)
        val modified = modifyBody(
          tx,
          curBody =>
              curBody
                  .copy(inputs = curBody.inputs ++ input, outputs = curBody.outputs :+ Sized(out))
        )
        TxBalance.doBalance(modified)(inputs.state, env.protocolParams, changeReturn, feePayer)
    }).get

    def withEnvironment(env: Environment): PayTo = copy(environment = Some(env))

    def withDatum(hash: DataHash): PayTo = copy(datum = Some(DatumOption.Hash(hash)))

    def withDatum(datum: Data): PayTo = copy(datum = Some(DatumOption.Inline(datum)))

    def usingInputs(in: TransactionInput): PayTo = {
        val resolvedUtxoO = transactionResolver
            .resolve(in)
            .map(in -> _)
        resolvedUtxoO match {
            case Some(u) =>
                val newResolvedInputs =
                    resolvedInputs.fold(ResolvedInputs.from(u))(_.add(u))
                copy(resolvedInputs = Some(newResolvedInputs))
            case None => this
        }
    }

    def usingFeePayerStrategy(feePayerStrategy: FeePayerStrategy): PayTo =
        copy(feePayerStrategy = Some(feePayerStrategy))

    def usingChangeReturnStrategy(changeReturnStrategy: ChangeReturnStrategy): PayTo =
        copy(changeReturnStrategy = Some(changeReturnStrategy))

    def modifyBody(tx: Transaction, f: TransactionBody => TransactionBody): Transaction = {
        val newBody = f(tx.body.value)
        tx.copy(body = KeepRaw(newBody))
    }
}

extension (output: TransactionOutput) {
    def -(coin: Coin): TransactionOutput = {
        val newValue = output.value - Value(coin)
        output match {
            case shelley: TransactionOutput.Shelley => shelley.copy(value = newValue)
            case babbage: TransactionOutput.Babbage => babbage.copy(value = newValue)
        }
    }

    def +(coin: Coin): TransactionOutput = {
        val newValue = output.value + Value(coin)
        output match {
            case shelley: TransactionOutput.Shelley => shelley.copy(value = newValue)
            case babbage: TransactionOutput.Babbage => babbage.copy(value = newValue)
        }
    }
}
