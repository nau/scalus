package scalus.bloxbean

import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import org.scalatest.funsuite.AnyFunSuite
import scalus.Compiler
import scalus.builtin.{ByteString, Data, FromData}
import scalus.cardano.address.*
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.{CardanoMutator, Context, State, UtxoEnv}
import scalus.cardano.ledger.txbuilder.*
import scalus.cardano.ledger.txbuilder.ResolvedTxInput.Pubkey
import scalus.cardano.onchain.OnchainError
import scalus.ledger.api.v2.ScriptContext
import scalus.ledger.api.{MajorProtocolVersion, Timelock}
import scalus.ledger.babbage.ProtocolParams
import scalus.uplc.eval.ExBudget
import scalus.toUplc
import scalus.plutusV3
import scalus.uplc.Program

import scala.collection.immutable.SortedMap

def resolvedTransaction(txHash: String, address: Address) = {
    TransactionInput(
      TransactionHash.fromHex(txHash),
      0
    ) -> TransactionOutput(address, Value.lovelace(10_000_000_000L))
}

/*
 * Expects yaci dev kit (with store) to exist. Relies on values that ` yaci-devkit up --enable-yaci-store` has on chain by default.
 * Heavy WIP, but tx passes.
 *
 * Todo:
 *  make prettier
 */
class TxBuilderIntegrationTest extends AnyFunSuite {

    private val EXISTING_UTXO = "6d36c0e2f304a5c27b85b3f04e95fc015566d35aef5f061c17c70e3e8b9ee508"
    private val SPENDER_ADDRESS = Address.fromBech32(
      "addr_test1qryvgass5dsrf2kxl3vgfz76uhp83kv5lagzcp29tcana68ca5aqa6swlq6llfamln09tal7n5kvt4275ckwedpt4v7q48uhex"
    )
    private val SPENDER_PAYMENT_KEY =
        "ed25519e_sk1sqr5ymxr5377q2tww7qzj8wdf9uwsx530p6txpfktvdsjvh2t3dk3q27c7gkel6anmfy4a2g6txy0f4mquwmj3pppvy3046006ulussa20jpu"
    private val TARGET_ADDRESS =
        Address.fromBech32(
          "addr_test1qpqy3lufef8c3en9nrnzp2svwy5vy9zangvp46dy4qw23clgfxhn3pqv243d6wptud7fuaj5tjqer7wc7m036gx0emsqaqa8te"
        )

    private val MNEMONIC =
        "test test test test test test test test test test test test test test test test test test test test test test test sauce"
    private val DERIVATION = "m/1852'/1815'/0'/0/0"

    def fetchProtocolParams(): ProtocolParams = {
        import upickle.default.*

        import java.net.URI
        import java.net.http.{HttpClient, HttpRequest, HttpResponse}

        val httpClient = HttpClient.newBuilder().build()
        val request = HttpRequest
            .newBuilder()
            .uri(URI.create(s"http://localhost:10000/local-cluster/api/epochs/parameters"))
            .GET()
            .build()

        val response = httpClient.send(request, HttpResponse.BodyHandlers.ofString())
        if response.statusCode() == 200 then {
            read[ProtocolParams](response.body())(using ProtocolParams.blockfrostParamsRW)
        } else {
            throw new Exception(response.body())
        }

    }

    def submitTransactionToCardano(cborBytes: Array[Byte]) = {
        val backendService = new BFBackendService("http://localhost:10000/local-cluster/api/", "")

        val result = backendService.getTransactionService.submitTransaction(cborBytes)
        assert(result.isSuccessful)
    }

    test("build and submit transaction") {
        pending
        val params = fetchProtocolParams()
        val costModels = CostModels.fromProtocolParams(params)
        val evaluator = PlutusScriptEvaluator(
          SlotConfig.Mainnet,
          initialBudget = ExBudget.enormous,
          protocolMajorVersion = MajorProtocolVersion.plominPV,
          costModels = costModels
        )

        val environment = Environment(
          protocolParams = params,
          evaluator = evaluator,
          network = Network.Testnet
        )

        val utxo = List(resolvedTransaction(EXISTING_UTXO, SPENDER_ADDRESS)).toMap

        val inputSelector = InputSelector(
          Set(ResolvedTxInput.Pubkey(utxo.head)),
          collateral = Set.empty
        )

        val changeReturnStrategy = ChangeReturnStrategy.toAddress(SPENDER_ADDRESS)
        val feePayerStrategy = FeePayerStrategy.subtractFromAddress(SPENDER_ADDRESS)

        val interpreter = InterpreterWithProvidedData(
          inputSelector = inputSelector,
          utxo = utxo,
          environment = environment,
          changeReturnStrategy = changeReturnStrategy,
          feePayerStrategy = feePayerStrategy,
          evaluator = environment.evaluator
        )

        val paymentIntention = Intention.Pay(
          address = TARGET_ADDRESS,
          value = Value(Coin(2_000_000L))
        )

        val tx = interpreter.realize(paymentIntention)
        val signed = makeSignerFrom(DERIVATION, MNEMONIC)
            .signTx(tx)

        val cborBytes = signed.toCbor
        val scalusLedgerRulesVerificationResult = CardanoMutator(
          Context(signed.body.value.fee, UtxoEnv(0L, params, CertState.empty)),
          State(utxo, CertState.empty),
          signed
        )
        assert(scalusLedgerRulesVerificationResult.isRight)
        submitTransactionToCardano(cborBytes)
    }

    test("build and submit transactions that spend script inputs") {
        pending
        val validator: Program = Compiler
            .compile((scriptContext: Data) => {
                val context = scriptContext.to[ScriptContext]
                throw new OnchainError("haha")
            })
            .toUplc(generateErrorTraces = true, debug = true)
            .plutusV3

        val params = fetchProtocolParams()
        val costModels = CostModels.fromProtocolParams(params)
        val evaluator = PlutusScriptEvaluator(
          SlotConfig.Mainnet,
          initialBudget = ExBudget.enormous,
          protocolMajorVersion = MajorProtocolVersion.plominPV,
          costModels = costModels
        )
        val environment = Environment(
          protocolParams = params,
          evaluator = evaluator,
          network = Network.Testnet
        )

        val script = Script.PlutusV3(validator.cborByteString)

        val scriptAddress = ShelleyAddress(
          Network.Testnet,
          ShelleyPaymentPart.scriptHash(script.scriptHash),
          ShelleyDelegationPart.Null
        )

        val changeUtxo = TransactionInput(
          TransactionHash.fromHex(
            "07d181fe781e5e323dcb78c75f58d5af06f1151463a30dadb2c004bc31ba7208"
          ),
          0
        ) -> TransactionOutput(SPENDER_ADDRESS, Value.lovelace(9997832739L))
        val scriptUtxo = TransactionInput(
          TransactionHash.fromHex(
            "7c3e43cf506af24ec53d06f6b93dbb7a21ec84f8df465b80be66779f24e95581"
          ),
          0
        ) -> TransactionOutput(scriptAddress, Value.lovelace(2000000L), None)

        val interpreter = InterpreterWithProvidedData(
          InputSelector(
            paymentInputs = Set(ResolvedTxInput.Script(scriptUtxo, script, Data.unit)),
            collateral = Set(Pubkey(changeUtxo))
          ),
          Map(scriptUtxo, changeUtxo),
          environment,
          ChangeReturnStrategy.toAddress(TARGET_ADDRESS),
          FeePayerStrategy.subtractFromFirstOutput,
          evaluator
        )

        val tx = interpreter
            .realize(
              Intention.Pay(
                Address.fromBech32(
                  "addr_test1qpqy3lufef8c3en9nrnzp2svwy5vy9zangvp46dy4qw23clgfxhn3pqv243d6wptud7fuaj5tjqer7wc7m036gx0emsqaqa8te"
                ),
                Value.lovelace(1000000)
              )
            )

        println(
          CardanoMutator(
            Context(tx.body.value.fee, UtxoEnv(0L, params, CertState.empty)),
            State(Map(scriptUtxo, changeUtxo), CertState.empty),
            tx
          )
        )
    }

//    test("build and submit plutus script minting transaction") {
//        val params = fetchProtocolParams()
//        val costModels = CostModels.fromProtocolParams(params)
//        val evaluator = PlutusScriptEvaluator(
//          SlotConfig.Mainnet,
//          initialBudget = ExBudget.enormous,
//          protocolMajorVersion = MajorProtocolVersion.plominPV,
//          costModels = costModels
//        )
//
//        val environment = Environment(
//          protocolParams = params,
//          evaluator = evaluator,
//          network = Network.Testnet
//        )
//
//        val spenderAddress = Address.fromBech32(SPENDER_ADDRESS)
//
//        val keyHash = spenderAddress.asInstanceOf[ShelleyAddress].payment.asInstanceOf[ShelleyPaymentPart.Key].hash
//        val nativeScript = Script.Native(Timelock.Signature(keyHash))
//        val policyId = nativeScript.scriptHash
//
//        // Define the asset to mint
//        val assetName = AssetName(ByteString.fromString("co2"))
//        val mintAmount = 1000L
//        val mintValue = Mint(MultiAsset(SortedMap(policyId -> SortedMap(assetName -> mintAmount))))
//
//        val utxo = List(resolvedTransaction(EXISTING_UTXO, spenderAddress)).toMap
//
//        val inputSelector = InputSelector(
//          Set(ResolvedTxInput.Pubkey(utxo.head)),
//          collateral = Set.empty
//        )
//
//        val changeReturnStrategy = ChangeReturnStrategy.toAddress(spenderAddress)
//        val feePayerStrategy = FeePayerStrategy.subtractFromAddress(spenderAddress)
//
//        val interpreter = InterpreterWithProvidedData(
//          inputSelector = inputSelector,
//          utxo = utxo,
//          environment = environment,
//          changeReturnStrategy = changeReturnStrategy,
//          feePayerStrategy = feePayerStrategy,
//          evaluator = environment.evaluator
//        )
//
//        val recipientAddress = Address.fromBech32(TARGET_ADDRESS)
//        val mintingIntention = Intention.Mint(
//          mintValue = mintValue,
//          mintIntention = MintIntention.UsingNative(nativeScript),
//          toAddress = recipientAddress
//        )
//
//        val tx = interpreter.realize(mintingIntention)
//        val signed = makeSignerFrom(DERIVATION, MNEMONIC)
//            .signTx(tx)
//
//        // Verify the transaction contains the expected mint
//        assert(signed.body.value.mint.isDefined)
//        val mint = signed.body.value.mint.get
//        assert(mint.assets.contains(policyId))
//        assert(mint.assets(policyId).contains(assetName))
//        assert(mint.assets(policyId)(assetName) == mintAmount)
//
//        // Verify the recipient receives the minted tokens
//        val targetOutput = signed.body.value.outputs.find(_.value.address == recipientAddress)
//        assert(targetOutput.isDefined)
//        assert(targetOutput.get.value.value.assets.assets.contains(policyId))
//        assert(targetOutput.get.value.value.assets.assets(policyId).contains(assetName))
//        assert(targetOutput.get.value.value.assets.assets(policyId)(assetName) == mintAmount)
//
//        // Verify native script is included in witness set
//        assert(signed.witnessSet.nativeScripts.contains(nativeScript))
//        assert(signed.witnessSet.redeemers.isEmpty) // Native scripts don't use redeemers
//
//        val cborBytes = signed.toCbor
//        val scalusLedgerRulesVerificationResult = CardanoMutator(
//          Context(signed.body.value.fee, UtxoEnv(0L, params, CertState.empty)),
//          State(utxo, CertState.empty),
//          signed
//        )
//        assert(scalusLedgerRulesVerificationResult.isRight)
//        submitTransactionToCardano(cborBytes)
//    }

}
