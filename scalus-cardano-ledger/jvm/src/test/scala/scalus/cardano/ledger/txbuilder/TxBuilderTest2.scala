package scalus.cardano.ledger.txbuilder

import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.{Address, ArbitraryInstances as ArbAddresses, Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.{ArbitraryInstances as ArbLedger, CostModels, DatumOption, PlutusScript, PlutusScriptEvaluator, Script, SlotConfig, TransactionHash, TransactionInput, TransactionOutput, UTxO, Value}
import scalus.ledger.api.MajorProtocolVersion
import scalus.ledger.babbage.ProtocolParams
import scalus.uplc.eval.ExBudget
import upickle.default.read
import scalus.cardano.ledger.txbuilder.assemble

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
        val myAddress = arbitrary[Address].sample.get
        val faucet = arbitrary[Address].sample.get
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

        val dummyResolver = new TransactionResolver {
            override def resolveScript(
                input: TransactionInput,
                script: PlutusScript,
                redeemer: Data,
                data: Option[DatumOption]
            ): ResolvedTxInput.Script = ???

            override def resolvePubkey(
                input: TransactionInput,
                data: Option[DatumOption]
            ): ResolvedTxInput.Pubkey = ???
        }
        val intention: Intention.Pay = Intention.Pay(faucet, payment)
        assemble(intention)(
          EnvironmentGetter(env),
          collateral.keySet,
          Set(ResolvedTxInput.Script(txInputs.head, script, Data.unit)),
          dummyResolver,
          evaluator
        ).assemble()
            .build(
              FeePayerStrategy.subtractFromAddress(myAddress),
              ChangeReturnStrategy.toAddress(myAddress)
            )

    }
}
