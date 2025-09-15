package scalus.bloxbean

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.backend.api.*
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.common.model.Networks
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.{ByteString, Data, platform}
import scalus.cardano.address.*
import scalus.cardano.ledger.txbuilder.{BuilderContext, Environment, StakingTransactionBuilder}
import scalus.cardano.ledger.{AddrKeyHash, AssetName, Coin, CostModels, MultiAsset, PlutusScriptEvaluator, Script, SlotConfig, Value}
import scalus.ledger.api.{MajorProtocolVersion, Timelock}
import scalus.ledger.api.v1.{CurrencySymbol, TokenName}
import scalus.ledger.api.v3.ScriptContext
import scalus.ledger.babbage.ProtocolParams
import scalus.prelude.orFail
import scalus.uplc.Program
import scalus.uplc.eval.ExBudget
import scalus.{Compiler, plutusV3, toUplc}

import scala.collection.immutable.SortedMap
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}

class TransactionBuilderIntegrationTest extends AnyFunSuite {

    // Reuse the same addresses and setup from TxBuilderIntegrationTest
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
    private lazy val backendService = createBackendService()
    private lazy val environment = createEnvironment()

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

    // Reuse the fetchProtocolParams method from TxBuilderIntegrationTest
    def fetchProtocolParams(): ProtocolParams = {
        import upickle.default.*

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

    final class DevkitCompositeBackend(
        storeBase: String = "http://localhost:8080/api/v1/",
        clusterBase: String = "http://localhost:10000/local-cluster/api/"
    ) extends BackendService {

        private val store = new BFBackendService(storeBase, "")
        private val cluster = new BFBackendService(clusterBase, "")

        // block/tx/utxo/address/asset/script/pool/metadata -> store
        override def getAssetService: AssetService = store.getAssetService

        override def getBlockService: BlockService = store.getBlockService

        override def getTransactionService: TransactionService = store.getTransactionService

        override def getUtxoService: UtxoService = store.getUtxoService

        override def getAddressService: AddressService = store.getAddressService

        override def getAccountService: AccountService = store.getAccountService

        override def getScriptService: ScriptService = store.getScriptService

        override def getPoolService: PoolService = store.getPoolService

        override def getMetadataService: MetadataService = store.getMetadataService

        // epoch + network info (protocol params) -> cluster
        override def getEpochService: EpochService = cluster.getEpochService

        override def getNetworkInfoService: NetworkInfoService = cluster.getNetworkInfoService
    }

    // convenience factory mirroring your signature
    def createBackendService(): BackendService = new DevkitCompositeBackend()

    // Reuse the submitTransactionToCardano method from TxBuilderIntegrationTest
    def submitTransactionToCardano(transaction: scalus.cardano.ledger.Transaction) = {

        val cborBytes = scalus.Cbor.encode(transaction)
        val result = backendService.getTransactionService.submitTransaction(cborBytes)
        if result.isSuccessful then succeed
        else fail(s"Error during tx submission: ${result.getResponse}")
    }

    test("simple pay to address") {
        val network = Networks.testnet()
        val account = new Account(network, MNEMONIC)

        val paymentAmount = Value.lovelace(5_000_000L)
        val tx = BuilderContext(
            environment.protocolParams,
            environment.evaluator,
            environment.network,
            backendService = backendService
        ).buildNewTx
            .withPayer(SPENDER_ADDRESS)
            .payTo(TARGET_ADDRESS, paymentAmount)
            .buildAndSign(makeSignerFrom(DERIVATION, MNEMONIC))

        submitTransactionToCardano(tx)

        println("CCL transaction submitted successfully")
    }

    test("pay from a native script") {
        val network = Networks.testnet()
        val account = new Account(network, MNEMONIC)
        val keyBytes = account.publicKeyBytes()
        val keyHash = AddrKeyHash(platform.blake2b_224(ByteString.fromArray(keyBytes)))

        val signatureTimelock = Timelock.Signature(keyHash)
        val nativeScript = Script.Native(signatureTimelock)

        val scriptAddress = ShelleyAddress(
            Network.Testnet,
            ShelleyPaymentPart.scriptHash(nativeScript.scriptHash),
            ShelleyDelegationPart.Null
        )

        val paymentAmount = Value.lovelace(5_000_000L)
        val context = BuilderContext(
            environment.protocolParams,
            environment.evaluator,
            environment.network,
            backendService = backendService
        )

        val tx1 = context.buildNewTx
            .withPayer(SPENDER_ADDRESS)
            .payTo(scriptAddress, paymentAmount)
            .buildAndSign(makeSignerFrom(DERIVATION, MNEMONIC))

        println("Transferring to native script...")
        submitTransactionToCardano(tx1)
        println("Success!")
        println("Transferring from native script...")

        Thread.sleep(1_000)

        val tx2 = context.buildNewTx
            .withPayer(scriptAddress)
            .withAttachedNativeScript(nativeScript)
            .payTo(SPENDER_ADDRESS, Value.lovelace(3_000_000L))
            .buildAndSign(makeSignerFrom(DERIVATION, MNEMONIC))

        submitTransactionToCardano(tx2)
        println("Native script spending transaction submitted successfully")
    }

    test("mint using a native policy") {
        val network = Networks.testnet()
        val account = new Account(network, MNEMONIC)
        val keyBytes = account.publicKeyBytes()
        val keyHash = AddrKeyHash(platform.blake2b_224(ByteString.fromArray(keyBytes)))

        val signatureTimelock = Timelock.Signature(keyHash)
        val nativeScript = Script.Native(signatureTimelock)

        val policyId = nativeScript.scriptHash

        val tokenName = AssetName(ByteString.fromString("co2"))
        val tokenAmount = 1000L
        val tokens = MultiAsset(
            SortedMap(policyId -> SortedMap(tokenName -> tokenAmount))
        )

        val context = BuilderContext(
            environment.protocolParams,
            environment.evaluator,
            environment.network,
            backendService = backendService
        )

        println("Minting co2 using a native script...")
        val tx = context.buildNewTx
            .withPayer(SPENDER_ADDRESS)
            .withAttachedNativeScript(nativeScript)
            .mint(tokens, TARGET_ADDRESS)
            .buildAndSign(makeSignerFrom(DERIVATION, MNEMONIC))

        submitTransactionToCardano(tx)
        println("Success!")
    }

    inline given opts: scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = false,
      optimizeUplc = true,
    )

    test("pay from a plutus script") {
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

                val outs: scalus.prelude.SortedMap[CurrencySymbol, scalus.prelude.SortedMap[TokenName, BigInt]] =
                    context.txInfo.outputs.head.value.toSortedMap
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

        val network = Networks.testnet()
        val account = new Account(network, MNEMONIC)

        val paymentAmount = Value.lovelace(10_000_000L)
        val context = BuilderContext(
          environment.protocolParams,
          environment.evaluator,
          environment.network,
          backendService = backendService
        )
        val tx1 = context.buildNewTx
            .withPayer(SPENDER_ADDRESS)
            .payTo(scriptAddress, paymentAmount)
        .buildAndSign(makeSignerFrom(DERIVATION, MNEMONIC))

        println("Transferring to script...")
        submitTransactionToCardano(tx1)
        println("Success!")
        println("Transferring from script...")
        Thread.sleep(1_000)
        val tx2 = context.buildNewTx
            .withPayer(scriptAddress)
            .withAttachedScript(script)
            .payTo(SPENDER_ADDRESS, Value.lovelace(5_000_002L))
            .setCollateralPayer(SPENDER_ADDRESS)
            .buildAndSign(makeSignerFrom(DERIVATION, MNEMONIC)) // to be able to spend collaterals
        submitTransactionToCardano(tx2)
        println("Success!")
    }

    test("mint using a plutus script") {
        val co2OnlyMinting: Program = Compiler
            .compile((scriptContext: Data) => {
                val context = scriptContext.to[ScriptContext]

                val mint = context.txInfo.mint.toSortedMap

                mint.forall { case (policyId, tokens) =>
                    tokens.forall { case (tokenName, amount) =>
                        tokenName == ByteString.fromString("co2") && amount > BigInt(0)
                    }
                }.orFail("Only co2 tokens can be minted")
            })
            .toUplc()
            .plutusV3

        val plutusScript = Script.PlutusV3(co2OnlyMinting.cborByteString)

        val policyId = plutusScript.scriptHash

        val tokenName = AssetName(ByteString.fromString("co2"))
        val tokenAmount = 500L
        val tokens = MultiAsset(
            SortedMap(policyId -> SortedMap(tokenName -> tokenAmount))
        )

        val context = BuilderContext(
            environment.protocolParams,
            environment.evaluator,
            environment.network,
            backendService = backendService
        )

        println("Minting co2 tokens using a plutus script...")
        val tx = context.buildNewTx
            .withPayer(SPENDER_ADDRESS)
            .withAttachedScript(plutusScript)
            .mint(tokens, TARGET_ADDRESS)
            .setCollateralPayer(SPENDER_ADDRESS)
            .buildAndSign(makeSignerFrom(DERIVATION, MNEMONIC))

        submitTransactionToCardano(tx)
        println("Success!")
    }

    test("register stake address") {
        val network = Networks.testnet()
        val account = new Account(network, MNEMONIC)
        
        // Create a stake address from the same account
        val stakeKeyHash = AddrKeyHash(platform.blake2b_224(ByteString.fromArray(account.stakeHdKeyPair().getPublicKey.getKeyData)))
        val stakeAddress = StakeAddress(Network.Testnet, StakePayload.fromBytes(stakeKeyHash.bytes, false).get)
        
        val context = BuilderContext(
            environment.protocolParams,
            environment.evaluator,
            environment.network,
            backendService = backendService
        )

        println("Registering stake address...")
        val tx = StakingTransactionBuilder(context, SPENDER_ADDRESS, stakeAddress)
            .registerStakeAddress

        val signed = makeSignerFrom(DERIVATION, MNEMONIC).signTx(tx)
        submitTransactionToCardano(signed)
        println("Success!")
    }

    test("withdraw staking rewards") {
        val network = Networks.testnet()
        val account = new Account(network, MNEMONIC)
        
        // Create a stake address from the same account
        val stakeKeyHash = AddrKeyHash(platform.blake2b_224(ByteString.fromArray(account.stakeHdKeyPair().getPublicKey.getKeyData)))
        val stakeAddress = StakeAddress(Network.Testnet, StakePayload.fromBytes(stakeKeyHash.bytes, false).get)
        
        val context = BuilderContext(
            environment.protocolParams,
            environment.evaluator,
            environment.network,
            backendService = backendService
        )

        println("Withdrawing staking rewards...")
        val tx = StakingTransactionBuilder(context, SPENDER_ADDRESS, stakeAddress)
            .withdraw(500)

        val signed = makeSignerFrom(DERIVATION, MNEMONIC).signTx(tx)
        try {
            submitTransactionToCardano(signed)
        } catch {
            case e if e.getMessage.contains("WithdrawalsNotInRewards") =>
                pending // no rewards in account

        }
        println("Success!")
    }
}
