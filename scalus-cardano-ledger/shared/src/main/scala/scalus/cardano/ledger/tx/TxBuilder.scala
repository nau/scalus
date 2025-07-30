package scalus.cardano.ledger.tx
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.utils.MinTransactionFee
import scalus.cardano.ledger.*
import scalus.ledger.babbage.ProtocolParams
import upickle.default.read

import scala.annotation.tailrec

case class TxBuilder(
    protocolParams: ProtocolParams,
    network: Network,
    soFar: Transaction = TxBuilder.emptyTx,
    utxoProvider: Option[UtxoProvider] = None,
) {

    private def utxos = utxoProvider.get.utxos

    def payTo(address: Address): PayTo = PayTo(address, this)

    def signedBy(vk: VKeyWitness): TxBuilder = {
        val wSet = soFar.witnessSet.copy(vkeyWitnesses = soFar.witnessSet.vkeyWitnesses + vk)
        copy(soFar = soFar.copy(witnessSet = wSet))
    }

    def balanceAndCalculateFees(changeAddress: Address): Transaction = {
        @tailrec
        def go(currentTx: Transaction): Transaction = {
            val txBody = currentTx.body.value
            val consumed = calculateConsumedValue(txBody)
            val produced = calculateProducedValue(txBody)
            val fee = currentTx.body.value.fee
            val diff = consumed.coin - produced.coin

            if diff == Coin.zero then {
                currentTx
            } else if diff > Coin.zero then {
                val change = TransactionOutput(changeAddress, Value(diff))
                val newOutputs = txBody.outputs :+ Sized(change)
                val newTx = currentTx.copy(body = KeepRaw(txBody.copy(outputs = newOutputs)))
                MinTransactionFee(newTx, utxos, protocolParams) match {
                    case Right(correctFee) =>
                        go(newTx.copy(body = KeepRaw(newTx.body.value.copy(fee = correctFee))))
                    case Left(error) =>
                        throw new IllegalStateException(
                          s"Failed to calculate fees: $error. tx: $currentTx"
                        )
                }
            } else throw new IllegalStateException("does not converge")
        }
        go(soFar)
    }

    private def calculateConsumedValue(txBody: TransactionBody): Value = txBody.inputs
        .flatMap(utxos.get)
        .foldLeft(Value.zero)((acc, v) => acc + v.value)

    private def calculateProducedValue(txBody: TransactionBody): Value =
        txBody.outputs
            .map(_.value.value)
            .foldLeft(Value.zero)(_ + _) + Value(txBody.fee)
}

object TxBuilder {
    private val emptyTx: Transaction = Transaction(
      TransactionBody(Set.empty, IndexedSeq.empty, Coin.zero),
      TransactionWitnessSet.empty
    )
}

case class PayTo(
    destination: Address,
    builder: TxBuilder,
    count: Option[Value] = None,
    utxoProvider: Option[UtxoProvider] = None,
) {
    def amount(value: Value): PayTo = copy(count = Some(value))

    def using(utxos: UTxO): PayTo =
        copy(utxoProvider = Some(UtxoProvider.givenUtxos(utxos)))

    def using(address: Address): PayTo =
        copy(utxoProvider = Some(UtxoProvider.utxosFromAddress(address, targetValue = count)))

    def prepareTx: Either[String, TxBuilder] = {
        for {
            utxos <- utxoProvider.map(_.utxos).toRight("No UTXO provider specified")
            amount <- count.toRight("No amount specified")
        } yield {
            val output = TransactionOutput(destination, amount)
            val newOutputs = builder.soFar.body.value.outputs :+ Sized(output)
            val newInputs = builder.soFar.body.value.inputs ++ utxos.keys
            val newBody = builder.soFar.body.value.copy(
              inputs = newInputs,
              outputs = newOutputs
            )
            val newTx = builder.soFar.copy(body = builder.soFar.body.copy(value = newBody))
            builder.copy(soFar = newTx, utxoProvider = utxoProvider)
        }
    }

}

object Api {
    def main(args: Array[String]): Unit = {
        val params: ProtocolParams = read[ProtocolParams](
          this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
        )(using ProtocolParams.blockfrostParamsRW)
        val myAddress = Address.fromString(
          "addr_test1qp462993av9uxkrlc5ke8cltze7u435yrdhtfuylcpxsve290xc2fdt7tszwd829m0depehw2ewkzwjyv0gus7mzk6vqslffwq"
        )
        val faucet =
            Address.fromString("addr_test1vqeux7xwusdju9dvsj8h7mca9aup2k439kfmwy773xxc2hcu7zy99")
        val hash = TransactionHash.fromHex(
          "147f02a87b363ee674c4924b6119ac47b0b99e72522212d9b4f1c6cecb4840b4"
        )
        val utxo: UTxO = Map(
          TransactionInput(hash, 0) -> TransactionOutput(
            myAddress,
            Value.lovelace(9_994_832_739L)
          )
        )

        TxBuilder(params, Network.Testnet)
            .payTo(faucet)
            .amount(Value.lovelace(10_000_000L))
            .using(utxo)
            .prepareTx
            .right
            .get
            .balanceAndCalculateFees(myAddress)
    }
}

trait UtxoProvider {
    def utxos: UTxO
}
object UtxoProvider {
    def givenUtxos(u: UTxO): UtxoProvider = new UtxoProvider {
        override def utxos: UTxO = u
    }

    def utxosFromAddress(address: Address, targetValue: Option[Value]) =
        new UtxosByAddress(address, targetValue)
}

class UtxosByAddress(owner: Address, targetValue: Option[Value]) extends UtxoProvider {
    override def utxos: UTxO = Map.empty
}
