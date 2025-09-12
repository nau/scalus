package scalus.cardano.ledger.txbuilder

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.{platform, ByteString}
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.ledger.api.MajorProtocolVersion
import scalus.ledger.babbage.ProtocolParams
import scalus.uplc.eval.ExBudget
import upickle.default.read

import scala.collection.immutable.SortedSet

class LowLevelTxBuilderTest extends AnyFunSuite {

    private val params: ProtocolParams = read[ProtocolParams](
      this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
    )(using ProtocolParams.blockfrostParamsRW)
    private val evaluator = PlutusScriptEvaluator(
      SlotConfig.Mainnet,
      initialBudget = ExBudget.fromCpuAndMemory(10_000000000L, 10_000000L),
      protocolMajorVersion = MajorProtocolVersion.plominPV,
      costModels = CostModels.fromProtocolParams(params)
    )

    test("testBalanceFeeChange") {
        val (utxo, tx) =
            mkTx(in = Coin(2000_000), output = Coin(200), fee = Coin(1_000000))

        val r = LowLevelTxBuilder.balanceFeeChange(tx, 0, params, utxo, evaluator)
        pprint.pprintln(r)
    }

    private def mkTx(in: Coin, output: Coin, fee: Coin) = {
        val input = TransactionInput(Hash(platform.blake2b_256(ByteString.fromString("asdf"))), 0)
        val addr =
            "addr1qxwg0u9fpl8dac9rkramkcgzerjsfdlqgkw0q8hy5vwk8tzk5pgcmdpe5jeh92guy4mke4zdmagv228nucldzxv95clqe35r3m"
        val utxo = Map(
          input -> TransactionOutput(
            address = Address.fromBech32(addr),
            value = Value(in)
          )
        )
        val tx = Transaction(
          TransactionBody(
            inputs = TaggedOrderedSet(SortedSet(input)),
            outputs = Vector(
              Sized(
                TransactionOutput(
                  address = Address.fromBech32(addr),
                  value = Value(output)
                )
              )
            ),
            fee = fee,
          ),
          witnessSet = TransactionWitnessSet.empty
        )
        (utxo, tx)
    }
}
