package scalus.cardano.ledger.txbuilder
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.{Address, ArbitraryInstances as ArbAddresses, Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.TransactionException.ValueNotConservedUTxOException
import scalus.cardano.ledger.{ArbitraryInstances as ArbLedger, Coin, CostModels, TransactionException, TransactionHash, TransactionInput, TransactionOutput, UTxO, Value, *}
import scalus.ledger.api.MajorProtocolVersion
import scalus.ledger.babbage.ProtocolParams
import scalus.uplc.eval.ExBudget
import upickle.default.read

class TxBuilderTest extends AnyFunSuite with ArbAddresses with ArbLedger {
    val params: ProtocolParams = read[ProtocolParams](
      this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
    )(using ProtocolParams.blockfrostParamsRW)
    private val costModels = CostModels.fromProtocolParams(params)

    test("should balance the transaction when the inputs exceed the outputs") {
        val myAddress = arbitrary[Address].sample.get
        val faucet = arbitrary[Address].sample.get
        val hash = arbitrary[TransactionHash].sample.get

        // Input tx produced 1_000_000 lovelace
        val availableLovelace = Value.lovelace(1_000_000L)
        val utxo: UTxO = Map(
          TransactionInput(hash, 0) -> TransactionOutput(
            myAddress,
            availableLovelace
          )
        )

        val paymentAmount = Value.lovelace(500L)
        val tx = TxBuilder
            .initialize(utxo, params, Network.Testnet)
            .payToAddress(faucet, paymentAmount)
            .doFinalize
        assert(tx.body.value.outputs.size == 2)
        assert(tx.body.value.outputs.exists(_.value.address == myAddress))
        assert(tx.body.value.outputs.exists(_.value.address == faucet))

        val sumOutputs = tx.body.value.outputs
            .map(_.value.value.coin)
            .foldLeft(Coin.zero)(_ + _)
        val fee = tx.body.value.fee
        assert(Value(sumOutputs + fee) == availableLovelace)

    }
    test("should throw when trying to create a transaction where outputs exceed the inputs") {
        val myAddress = arbitrary[Address].sample.get
        val faucet = arbitrary[Address].sample.get
        val hash = arbitrary[TransactionHash].sample.get

        // Input tx produced 1K lovelace
        val utxo: UTxO = Map(
          TransactionInput(hash, 0) -> TransactionOutput(
            myAddress,
            Value.lovelace(1_000L)
          )
        )
        // exceeds available lovelace
        val paymentAmount = 10_000L

        assertThrows[ValueNotConservedUTxOException] {
            TxBuilder
                .initialize(utxo, params, Network.Testnet)
                .payToAddress(faucet, Value.lovelace(paymentAmount))
                .doFinalize
        }
    }
    test(
      "should throw when trying to create a transaction where outputs cover the input, but don't cover the fee"
    ) {
        val myAddress = arbitrary[Address].sample.get
        val faucet = arbitrary[Address].sample.get
        val hash = arbitrary[TransactionHash].sample.get

        // Input tx produced 1_000_000 lovelace
        val availableLovelace = Value.lovelace(1_000_000L)
        val utxo: UTxO = Map(
          TransactionInput(hash, 0) -> TransactionOutput(
            myAddress,
            availableLovelace
          )
        )

        // Technically available, but not with a fee
        val paymentAmount = availableLovelace - Value.lovelace(1L)
        assertThrows[TransactionException.IllegalArgumentException] {
            val tx = TxBuilder
                .initialize(utxo, params, Network.Testnet)
                .payToAddress(faucet, paymentAmount)
                .doFinalize
        }

    }

    test("should balance script transaction with proper fees") {
        val emptyScriptBytes = Array(69, 1, 1, 0, 36, -103).map(_.toByte)
        val scriptBytes = ByteString.unsafeFromArray(emptyScriptBytes)
        val script = Script.PlutusV3(scriptBytes)

        val myAddress = ShelleyAddress(
          Network.Testnet,
          ShelleyPaymentPart.Key(arbitrary[AddrKeyHash].sample.get),
          ShelleyDelegationPart.Null
        )

        val hash = arbitrary[TransactionHash].sample.get

        val hugeCollateral = Map(
          TransactionInput(arbitrary[TransactionHash].sample.get, 0) -> TransactionOutput(
            myAddress,
            Value.lovelace(100_000_000L)
          )
        )
        val utxo: UTxO =
            val scriptAddress = ShelleyAddress(
              Network.Testnet,
              ShelleyPaymentPart.Script(script.scriptHash),
              ShelleyDelegationPart.Null
            )
            val availableLovelace = Value.lovelace(10_000_000L)
            Map(
              TransactionInput(hash, 0) -> TransactionOutput(scriptAddress, availableLovelace)
            ) ++ hugeCollateral

        val evaluator = PlutusScriptEvaluator(
          SlotConfig.Mainnet,
          initialBudget = ExBudget.enormous,
          protocolMajorVersion = MajorProtocolVersion.plominPV,
          costModels = costModels
        )

        val datum = Data.unit
        val redeemer = Data.unit

        val tx = ScriptTxBuilder
            .initialize(utxo, params, Network.Testnet)
            .payToAddress(myAddress, Value.lovelace(1_000_000L))
            .withScript(script, datum, redeemer, 0)
            .withCollateral(hugeCollateral)
            .doFinalizeScript(evaluator)

        assert(tx.body.value.outputs.nonEmpty)
        assert(tx.witnessSet.redeemers.isDefined)
        assert(tx.witnessSet.redeemers.get.value.toSeq.size == 1)

    }
}
