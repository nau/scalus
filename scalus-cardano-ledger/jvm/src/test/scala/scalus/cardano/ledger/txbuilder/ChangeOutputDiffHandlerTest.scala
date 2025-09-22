package scalus.cardano.ledger.txbuilder

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.{platform, ByteString}
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.{Context, State, ValueNotConservedUTxOValidator}
import scalus.cardano.ledger.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.cardano.ledger.txbuilder.TxBalancingError.InsufficientFunds
import scalus.uplc.eval.ExBudget

import scala.collection.immutable.SortedSet

class ChangeOutputDiffHandlerTest extends AnyFunSuite {

    private val params: ProtocolParams = ProtocolParams.fromBlockfrostJson(
      this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
    )
    private val evaluator = PlutusScriptEvaluator(
      SlotConfig.Mainnet,
      initialBudget = ExBudget.fromCpuAndMemory(10_000000000L, 10_000000L),
      protocolMajorVersion = MajorProtocolVersion.plominPV,
      costModels = CostModels.fromProtocolParams(params)
    )

    enum Expected {
        case Success(outputLovelace: Long, fee: Long)
        case Failure(error: TxBalancingError)
    }

    test("should fail when insufficient funds would require change output below minimum ADA") {
        check(
          in = 1_000_000,
          output = 800_000,
          fee = 200_000,
          expected = Expected.Failure(InsufficientFunds(0, 178370))
        )
    }

    test("should add change to output when excess funds are available") {
        check(
          in = 1_500_000,
          output = 1_000_000,
          fee = 200_000,
          expected = Expected.Success(outputLovelace = 1_300_000, fee = 200_000)
        )
    }

    test("should handle exact balance with no change needed") {
        check(
          in = 2_000_000,
          output = 1_800_000,
          fee = 200_000,
          expected = Expected.Success(outputLovelace = 1_800_000, fee = 200_000)
        )
    }

    test("should remove from change output for balancing") {
        check(
          in = 5_000_000,
          output = 5_000_000,
          fee = 200_000,
          expected = Expected.Success(outputLovelace = 4_800_000, fee = 200_000)
        )
    }

    test("should fail when output would become below minimum ADA") {
        val insufficientFunds: InsufficientFunds = InsufficientFunds(-160529, 138899)
        check(
          in = 1_000_000,
          output = 1_000_000,
          fee = 0,
          expected = Expected.Failure(insufficientFunds)
        )

        check(
          in = 1_000_000 + insufficientFunds.minRequired,
          output = 1_000_000,
          fee = 0,
          expected = Expected.Success(outputLovelace = 978_370, fee = -insufficientFunds.diff)
        )
    }

    test("should handle zero fee scenario with sufficient funds") {
        check(
          in = 3_000_000,
          output = 2_000_000,
          fee = 0,
          expected = Expected.Success(
            outputLovelace = 2_839471,
            fee = 160529
          )
        )
    }

    test("should handle maximum possible coin values") {
        val maxPossible = 45_000_000_000L
        check(
          in = maxPossible,
          output = maxPossible - 1_000_000,
          fee = 500_000,
          expected = Expected.Success(outputLovelace = 44_999_500_000L, fee = 500_000)
        )
    }

    test("should fail on invalid change output index") {
        val (utxo, tx) = mkTx(Coin(2_000_000), Coin(1_000_000), Coin(200_000))
        val handler = ChangeOutputDiffHandler(params, 5) // Invalid index > outputs.size

        try
            LowLevelTxBuilder.balanceFeeAndChange(
              tx,
              handler.changeOutputDiffHandler,
              params,
              utxo,
              evaluator
            )
        catch case err => assert(err.getMessage.contains("requirement failed"))
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

    private def check(
        in: Long,
        output: Long,
        fee: Long,
        expected: Expected
    ) = {
        val (utxo, tx) = mkTx(Coin(in), Coin(output), Coin(fee))

        val handler = ChangeOutputDiffHandler(params, 0)
        val r = LowLevelTxBuilder.balanceFeeAndChange(
          tx,
          handler.changeOutputDiffHandler,
          params,
          utxo,
          evaluator
        )
        (r, expected) match
            case (Right(value), Expected.Success(expectedValue, expectedFee)) =>
                assert(
                  ValueNotConservedUTxOValidator.validate(Context(), State(utxo), value).isRight
                )
                val body = value.body.value
                assert(body.fee.value == expectedFee)
                assert(body.outputs(0).value.value.coin.value == expectedValue)
            case (Left(err), Expected.Failure(expectedError)) =>
                assert(err == expectedError)
            case _ =>
                fail(s"Unexpected result: $r")

    }
}
