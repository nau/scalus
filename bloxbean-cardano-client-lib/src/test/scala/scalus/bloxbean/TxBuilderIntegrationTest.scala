package scalus.bloxbean

import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import org.scalatest.funsuite.AnyFunSuite
import scalus.Ignore
import scalus.builtin.Data
import scalus.cardano.address.*
import scalus.cardano.address.Network.Testnet
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.{CardanoMutator, Context, State, UtxoEnv}
import scalus.cardano.ledger.txbuilder.*
import scalus.cardano.ledger.txbuilder.Intention.Pay
import scalus.cardano.ledger.txbuilder.ResolvedTxInput.Pubkey
import scalus.ledger.api
import scalus.ledger.api.MajorProtocolVersion
import scalus.ledger.api.v1.{CurrencySymbol, TokenName}
import scalus.ledger.api.v3.ScriptContext
import scalus.ledger.babbage.ProtocolParams
import scalus.prelude.{orFail, SortedMap}
import scalus.uplc.Program
import scalus.uplc.eval.ExBudget

/*
 * Expects yaci dev kit (with store) to exist. Relies on values that ` yaci-devkit up --enable-yaci-store` has on chain by default.
 * Heavy WIP, but tx passes.
 */
class TxBuilderIntegrationTest extends AnyFunSuite {

    private val EXISTING_UTXO = "6d36c0e2f304a5c27b85b3f04e95fc015566d35aef5f061c17c70e3e8b9ee508"
    private val SPENDER_ADDRESS = Address.fromBech32(
      "addr_test1qryvgass5dsrf2kxl3vgfz76uhp83kv5lagzcp29tcana68ca5aqa6swlq6llfamln09tal7n5kvt4275ckwedpt4v7q48uhex"
    )
    private val TARGET_ADDRESS = Address.fromBech32(
      "addr_test1qpqy3lufef8c3en9nrnzp2svwy5vy9zangvp46dy4qw23clgfxhn3pqv243d6wptud7fuaj5tjqer7wc7m036gx0emsqaqa8te"
    )
    private val MNEMONIC =
        "test test test test test test test test test test test test test test test test test test test test test test test sauce"
    private val DERIVATION = "m/1852'/1815'/0'/0/0"

    private lazy val params = fetchProtocolParams()
    private lazy val environment = createEnvironment()

    def resolvedTransaction(
        txHash: String,
        address: Address,
        index: Int = 0,
        lovelace: Long = 10_000_000_000L
    ) = {
        TransactionInput(TransactionHash.fromHex(txHash), index) ->
            TransactionOutput(address, Value.lovelace(lovelace))
    }

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

    def createEnvironment(): Environment = {
        val costModels = CostModels.fromProtocolParams(params)
        val evaluator = PlutusScriptEvaluator(
          SlotConfig.Mainnet,
          initialBudget = ExBudget.enormous,
          protocolMajorVersion = MajorProtocolVersion.plominPV,
          costModels = costModels
        )
        Environment(protocolParams = params, evaluator = evaluator, network = Network.Testnet)
    }

    def createInterpreter(
        utxo: UTxO,
        wallet: Wallet,
        changeStrategy: ChangeReturnStrategy = ChangeReturnStrategy.toAddress(SPENDER_ADDRESS),
        feeStrategy: FeePayerStrategy = FeePayerStrategy.subtractFromAddress(SPENDER_ADDRESS)
    ): InterpreterWithProvidedData = {
        InterpreterWithProvidedData(
          wallet = wallet,
          environment = environment,
          changeReturnStrategy = changeStrategy,
          feePayerStrategy = feeStrategy,
          evaluator = environment.evaluator
        )
    }

    def validateAndSubmit(unsigned: Transaction, utxo: UTxO): Unit = {
        val signed = makeSignerFrom(DERIVATION, MNEMONIC).signTx(unsigned)
        val validationResult = CardanoMutator(
          Context(signed.body.value.fee, UtxoEnv(0L, params, CertState.empty, Testnet)),
          State(utxo, CertState.empty),
          signed
        )

        submitTransactionToCardano(signed.toCbor)
    }

    def submitTransactionToCardano(cborBytes: Array[Byte]) = {
        val backendService = new BFBackendService("http://localhost:10000/local-cluster/api/", "")
        val result = backendService.getTransactionService.submitTransaction(cborBytes)
        if result.isSuccessful then succeed
        else fail(s"Error during tx submission: ${result.getResponse}")
    }

    @Ignore
    inline given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
    )

    /** Refactor: 1) Transfer change from the previous test to the script. 2) Send the outputs from
      * the script back to the SPENDER
      */
    test("build and submit transactions that spend script inputs") {
        pending
        import scalus.*
        val luckyPaymentsOnly: Program = Compiler
            .compile((scriptContext: Data) => {
                def digitSum(bigInt: BigInt) = {
                    def loop(acc: BigInt, left: BigInt): BigInt = {
                        if left < 10 then {
                            acc + left
                        } else {
                            val mod = left % 10
                            loop(acc + mod, left / 10)
                        }
                    }
                    loop(BigInt(0), bigInt)

                }
                val context = scriptContext.to[ScriptContext]

                val outs: SortedMap[CurrencySymbol, SortedMap[TokenName, BigInt]] =
                    context.txInfo.outputs.last.value.toSortedMap
                val l = outs.toList
                val amount = outs.toList.head._2.toList.head._2
                val isLucky = digitSum(amount) % 7 == BigInt(0)
                isLucky.orFail("Lucky payments only.")
            })
            .toUplc()
            .plutusV3

        val script = Script.PlutusV3(luckyPaymentsOnly.cborByteString)

        val scriptAddress = ShelleyAddress(
          Network.Testnet,
          ShelleyPaymentPart.scriptHash(script.scriptHash),
          ShelleyDelegationPart.Null
        )

        def transferToScript = {
            val input = resolvedTransaction(EXISTING_UTXO, SPENDER_ADDRESS)
            val interpreter = createInterpreter(
              Map(input),
              Wallet.create(Set(ResolvedTxInput.Pubkey(input)), Set.empty)
            )
            val tx = interpreter.realize(Pay(scriptAddress, Value.lovelace(8_000_000_000L)))
            validateAndSubmit(tx, interpreter.wallet.utxo)
        }
        def transferFromScriptBackToSpender = {
            val collateral = resolvedTransaction(
              "77184f2a29ce22f4ab2e3c813c7400213dd69143b067fefe29af497bcb44b38d",
              SPENDER_ADDRESS,
              index = 0,
              1_999_832_389L
            )
            val input = resolvedTransaction(
              "77184f2a29ce22f4ab2e3c813c7400213dd69143b067fefe29af497bcb44b38d",
              scriptAddress,
              index = 1,
              8_000_000_000L
            )

            val interpreter = createInterpreter(
              Map(input, collateral),
              Wallet.create(
                paymentInputs = Set(ResolvedTxInput.Script(input, script, Data.unit)),
                collat = Set(Pubkey(collateral))
              ),
              changeStrategy = ChangeReturnStrategy.toAddress(scriptAddress),
              feeStrategy = FeePayerStrategy.subtractFromAddress(scriptAddress)
            )
            val tx = interpreter.realize(Pay(SPENDER_ADDRESS, Value.lovelace(5_000_000_036L)))

            validateAndSubmit(tx, interpreter.wallet.utxo)
        }

        println("Transferring to script...")
        transferToScript
        println("Success!")
        println("Transferring from script...")
        transferFromScriptBackToSpender
        println("Success!")
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
