package scalus.cardano.ledger.txbuilder

import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.{ArbitraryInstances as ArbAddresses, Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.{ArbitraryInstances as ArbLedger, AssetName, CostModels, Mint, MultiAsset, PlutusScriptEvaluator, Script, SlotConfig, TransactionHash, TransactionInput, TransactionOutput, UTxO, Value}
import scalus.ledger.api.MajorProtocolVersion
import scalus.ledger.babbage.ProtocolParams
import scalus.uplc.eval.ExBudget
import upickle.default.read

import scala.collection.immutable.SortedMap

class TxBuilderTest2 extends AnyFunSuite with ArbAddresses with ArbLedger {

    val params: ProtocolParams = read[ProtocolParams](
      this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
    )(using ProtocolParams.blockfrostParamsRW)

    private val costModels = CostModels.fromProtocolParams(params)
    private val evaluator = PlutusScriptEvaluator(
      SlotConfig.Mainnet,
      initialBudget = ExBudget.enormous,
      protocolMajorVersion = MajorProtocolVersion.plominPV,
      costModels = costModels
    )
    private val env = Environment(params, evaluator, Network.Mainnet)

    test("pay") {
        val myAddress = arbitrary[ShelleyAddress].sample.get
        val faucet = arbitrary[ShelleyAddress].sample.get
        val hash = arbitrary[TransactionHash].sample.get

        val emptyScriptBytes = Array(69, 1, 1, 0, 36, -103).map(_.toByte)
        val scriptBytes = ByteString.unsafeFromArray(emptyScriptBytes)
        val script = Script.PlutusV3(scriptBytes)

        val scriptAddress = ShelleyAddress(
          Network.Testnet,
          ShelleyPaymentPart.Script(script.scriptHash),
          ShelleyDelegationPart.Null
        )

        val availableLovelace = Value.lovelace(10_000_000L)
        val txInputs = Map(
          TransactionInput(hash, 0) -> TransactionOutput(scriptAddress, availableLovelace)
        )
        val collateral = Map(
          TransactionInput(arbitrary[TransactionHash].sample.get, 0) -> TransactionOutput(
            myAddress,
            Value.lovelace(100_000_000L)
          )
        )
        val utxo: UTxO = txInputs ++ collateral

        val payment = Value.lovelace(500L)

        val inputSelector = InputSelector(
          Set(ResolvedTxInput.Script(txInputs.head, script, Data.unit)),
          collateral.keySet
        )
        InterpreterWithProvidedData(
          inputSelector,
          utxo,
          env,
          ChangeReturnStrategy.toAddress(myAddress),
          FeePayerStrategy.subtractFromAddress(myAddress),
          evaluator
        ).realize(Intention.Pay(faucet, payment))

    }

    test("mint tokens with PlutusV3 script") {
        val myAddress = arbitrary[ShelleyAddress].sample.get
        val targetAddress = arbitrary[ShelleyAddress].sample.get
        val hash = arbitrary[TransactionHash].sample.get

        val scriptBytes = ByteString.unsafeFromArray(Array(69, 3, 1, 0, 36, -103).map(_.toByte))
        val mintingScript = Script.PlutusV3(scriptBytes)
        val policyId = mintingScript.scriptHash

        val assetName = AssetName(ByteString.fromString("co2"))
        val mintAmount = 1000L
        val mintValue = Mint(MultiAsset(SortedMap(policyId -> SortedMap(assetName -> mintAmount))))

        val availableLovelace = Value.lovelace(10_000_000L)
        val txInputs = Map(
          TransactionInput(hash, 0) -> TransactionOutput(myAddress, availableLovelace)
        )
        val collateral = Map(
          TransactionInput(arbitrary[TransactionHash].sample.get, 0) -> TransactionOutput(
            myAddress,
            Value.lovelace(100_000_000L)
          )
        )
        val utxo: UTxO = txInputs ++ collateral

        val inputSelector = InputSelector(
          Set(ResolvedTxInput.Script(txInputs.head, mintingScript, Data.unit)),
          collateral.keySet
        )

        val interpreter = InterpreterWithProvidedData(
          inputSelector,
          utxo,
          env,
          ChangeReturnStrategy.toAddress(myAddress),
          FeePayerStrategy.subtractFromAddress(myAddress),
          evaluator
        )

        val tx = interpreter.realize(
          Intention.Mint(mintValue, mintingScript, Data.unit, targetAddress)
        )

        assert(tx.body.value.mint.isDefined)
        val mint = tx.body.value.mint.get
        assert(mint.assets.contains(policyId))
        assert(mint.assets(policyId).contains(assetName))
        assert(mint.assets(policyId)(assetName) == mintAmount)

        val targetOutput = tx.body.value.outputs.find(_.value.address == targetAddress)
        assert(targetOutput.isDefined)
        assert(targetOutput.get.value.value.assets.assets.contains(policyId))
        assert(targetOutput.get.value.value.assets.assets(policyId).contains(assetName))
        assert(targetOutput.get.value.value.assets.assets(policyId)(assetName) == mintAmount)

        assert(tx.witnessSet.plutusV3Scripts.contains(mintingScript))
        assert(tx.witnessSet.redeemers.isDefined)
    }

    test("fail if the validator script throws an error") {
        val myAddress = arbitrary[ShelleyAddress].sample.get
        val faucet = arbitrary[ShelleyAddress].sample.get
        val hash = arbitrary[TransactionHash].sample.get

        val failingScriptCbor =
            "582c0101002533573892011f616c77617973206661696c696e672076616c696461746f722063616c6c6564001601"
        val script = Script.PlutusV3(ByteString.fromString(failingScriptCbor))

        val scriptAddress = ShelleyAddress(
          Network.Testnet,
          ShelleyPaymentPart.Script(script.scriptHash),
          ShelleyDelegationPart.Null
        )

        val availableLovelace = Value.lovelace(10_000_000L)
        val txInputs = Map(
          TransactionInput(hash, 0) -> TransactionOutput(scriptAddress, availableLovelace)
        )
        val collateral = Map(
          TransactionInput(arbitrary[TransactionHash].sample.get, 0) -> TransactionOutput(
            myAddress,
            Value.lovelace(100_000_000L)
          )
        )
        val utxo: UTxO = txInputs ++ collateral

        val payment = Value.lovelace(500L)

        val inputSelector = InputSelector(
          Set(ResolvedTxInput.Script(txInputs.head, script, Data.unit)),
          collateral.keySet
        )

        assertThrows[Exception] {
            InterpreterWithProvidedData(
              inputSelector,
              utxo,
              env,
              ChangeReturnStrategy.toAddress(myAddress),
              FeePayerStrategy.subtractFromAddress(myAddress),
              evaluator
            ).realize(Intention.Pay(faucet, payment))
        }
    }
}
