import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.backend.api.*
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.common.model.Networks
import com.bloxbean.cardano.client.crypto.bip32.HdKeyPair
import com.bloxbean.cardano.client.crypto.cip1852.{DerivationPath, Segment}
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.quicktx.{QuickTxBuilder, Tx}
import org.bouncycastle.crypto.digests.SHA512Digest
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.{platform, ByteString, Data}
import scalus.cardano.address.*
import scalus.cardano.ledger.txbuilder.{BuilderContext, Environment, StakingTransactionBuilder, TxSigner}
import scalus.cardano.ledger.*
import scalus.ledger.api.v1.{CurrencySymbol, TokenName}
import scalus.ledger.api.v3.ScriptContext
import scalus.ledger.api.{MajorProtocolVersion, Timelock}
import scalus.prelude.orFail
import scalus.uplc.Program
import scalus.uplc.eval.ExBudget
import scalus.{plutusV3, toUplc, Compiler}

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import scala.collection.immutable.SortedMap

class TransactionBuilderIntegrationTest extends AnyFunSuite {

    /*
     * Predefined accounts that exist in a fresh yaci devkit instance.
     */
    private val MNEMONIC =
        "test test test test test test test test test test test test test test test test test test test test test test test sauce"

    case class ExistingAccount(rawAddress: String, derivation: String) {
        def address = Address.fromBech32(rawAddress)
        def signer(getKeyPairs: Account => Seq[HdKeyPair] = a => Seq(a.hdKeyPair())) =
            makeSignerFrom(derivation, MNEMONIC)(getKeyPairs)
    }
    val account0 = ExistingAccount(
      rawAddress =
          "addr_test1qryvgass5dsrf2kxl3vgfz76uhp83kv5lagzcp29tcana68ca5aqa6swlq6llfamln09tal7n5kvt4275ckwedpt4v7q48uhex",
      derivation = "m/1852'/1815'/0'/0/0"
    )

    val account1 = ExistingAccount(
      rawAddress =
          "addr_test1qpqy3lufef8c3en9nrnzp2svwy5vy9zangvp46dy4qw23clgfxhn3pqv243d6wptud7fuaj5tjqer7wc7m036gx0emsqaqa8te",
      derivation = "m/1852'/1815'/1'/0/0"
    )

    val account2 = ExistingAccount(
      rawAddress =
          "addr_test1qr9xuxclxgx4gw3y4h4tcz4yvfmrt3e5nd3elphhf00a67xnrv5vjcv6tzehj2nnjj4cth4ndzyuf4asvvkgzeac2hfqk0za93",
      derivation = "m/1852'/1815'/2'/0/0"
    )

    val account3 = ExistingAccount(
      rawAddress =
          "addr_test1qqra0q073cecs03hr724psh3ppejrlpjuphgpdj7xjwvkqnhqttgsr5xuaaq2g805dldu3gq9gw7gwmgdyhpwkm59ensgyph06",
      derivation = "m/1852'/1815'/3'/0/0"
    )

    val account4 = ExistingAccount(
      rawAddress =
          "addr_test1qp38kfvcm4c39yt8sfgkp3tyqe736fz708xzxuy5s9w9ev43yh3sash5eeq9ngrfuzxrekpvmly52xlmyfy8lz39emhs2spswl",
      derivation = "m/1852'/1815'/4'/0/0"
    )

    val account5 = ExistingAccount(
      rawAddress =
          "addr_test1qrrv7774puml0exvzc0uqrc8axezy6a925kv4ucdx906qy6mhjxtmx44x70ndr7g6dgqcdaf69q8fnrdmtvfud5x7rsqvsuqx5",
      derivation = "m/1852'/1815'/5'/0/0"
    )

    val account6 = ExistingAccount(
      rawAddress =
          "addr_test1qpgkf2ccvu2uscmcqgy4dkyjeae0va3kk7yk04nuleekq3u3xrwgnnm6n0yfzz0e8x2kkehex2f6mexrjg9h9l2qhm4qkms53s",
      derivation = "m/1852'/1815'/6'/0/0"
    )

    val account7 = ExistingAccount(
      rawAddress =
          "addr_test1qq7a8p6zaxzgcmcjcy7ak8u5vn7qec9mjggzw6qg096nzlj6n7rflnv3x43vnv8q7q0h0ef4n6ncp5mljd2ljupwl79s5mqneq",
      derivation = "m/1852'/1815'/7'/0/0"
    )

    val account8 = ExistingAccount(
      rawAddress =
          "addr_test1qzyw0ensk3w2kgezk5vw77m0vmfs4mqdjg7ugclyvuy357g0vaukh8a8zgj09prvaw70f9gvz8sypmsjf5c0dctyhn2slcvsjn",
      derivation = "m/1852'/1815'/8'/0/0"
    )

    val account9 = ExistingAccount(
      rawAddress =
          "addr_test1qrrshpppv9uq95lj89tv4vv40cwnqmx5szzcndqhvr5hjfltl4s98wsjkwpg3v4k6h2vgcvdd2xwt82stq8fcmmsft6s8dzp8f",
      derivation = "m/1852'/1815'/9'/0/0"
    )

    val account10 = ExistingAccount(
      rawAddress =
          "addr_test1qrqfxwuz8rm0xvewfrp5eudgup24jsan8n22h3f6a7mavyjt0njqm4ykhhqzkdrq9ua8w0lhen8wsuuerexgsehn5u9syjlrxr",
      derivation = "m/1852'/1815'/10'/0/0"
    )

    val account11 = ExistingAccount(
      rawAddress =
          "addr_test1qp5l04egnh30q8x3uqn943d7jsa5za66htsvu6e74s8dacxwnjkm0n0v900d8mu20wlrx55xn07p8pm4fj0wdvtc9kwq7pztl7",
      derivation = "m/1852'/1815'/11'/0/0"
    )

    val account12 = ExistingAccount(
      rawAddress =
          "addr_test1qpcf5ursqpwx2tp8maeah00rxxdfpvf8h65k4hk3chac0fvu28duly863yqhgjtl8an2pkksd6mlzv0qv4nejh5u2zjsshr90k",
      derivation = "m/1852'/1815'/12'/0/0"
    )

    val account13 = ExistingAccount(
      rawAddress =
          "addr_test1qr79wm0n5fucskn6f58u2qph9k4pm9hjd3nkx4pwe54ds4gh2vpy4h4r0sf5ah4mdrwqe7hdtfcqn6pstlslakxsengsgyx75q",
      derivation = "m/1852'/1815'/13'/0/0"
    )

    val account14 = ExistingAccount(
      rawAddress =
          "addr_test1qqe5df3su6tjhuuve6rjr8d36ccxre7dxfx2mzxp3egy72vsetrga9el2yjke2fcdql6f6sjzu7h6prajs8mhzpm6r5qpkfq9m",
      derivation = "m/1852'/1815'/14'/0/0"
    )

    val account15 = ExistingAccount(
      rawAddress =
          "addr_test1qq5tscksq8n2vjszkdtqe0zn9645246ex3mu88x9y0stnlzjwyqgnrq3uuc3jst3hyy244rrwuxke0m7ezr3cn93u5vq0rfv8t",
      derivation = "m/1852'/1815'/15'/0/0"
    )

    val account16 = ExistingAccount(
      rawAddress =
          "addr_test1qp0qu4cypvrwn4c7pu50zf3x9qu2drdsk545l5dnsa7a5gsr6htafuvutm36rm23hdnsw7w7r82q4tljuh55drxqt30q6vm8vs",
      derivation = "m/1852'/1815'/16'/0/0"
    )

    val account17 = ExistingAccount(
      rawAddress =
          "addr_test1qqm87edtdxc7vu2u34dpf9jzzny4qhk3wqezv6ejpx3vgrwt46dz4zq7vqll88fkaxrm4nac0m5cq50jytzlu0hax5xqwlraql",
      derivation = "m/1852'/1815'/17'/0/0"
    )

    val account18 = ExistingAccount(
      rawAddress =
          "addr_test1qrzufj3g0ua489yt235wtc3mrjrlucww2tqdnt7kt5rs09grsag6vxw5v053atks5a6whke03cf2qx3h3g2nhsmzwv3sgml3ed",
      derivation = "m/1852'/1815'/18'/0/0"
    )

    val account19 = ExistingAccount(
      rawAddress =
          "addr_test1qrh3nrahcd0pj6ps3g9htnlw2jjxuylgdhfn2s5rxqyrr43yzewr2766qsfeq6stl65t546cwvclpqm2rpkkxtksgxuq90xn5f",
      derivation = "m/1852'/1815'/19'/0/0"
    )

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
            ProtocolParams.fromBlockfrostJson(response.body())
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

    def createBackendService(): BackendService = new DevkitCompositeBackend()

    def submitTransactionToCardano(
        context: BuilderContext,
        transaction: scalus.cardano.ledger.Transaction
    ) = {
        val cborBytes = scalus.Cbor.encode(transaction)
        val result = context.backendService.getTransactionService.submitTransaction(cborBytes)
        if result.isSuccessful then succeed
        else fail(s"Error during tx submission: ${result.getResponse}")
    }

    test("simple pay to address") {
        val network = Networks.testnet()
        val account = new Account(network, MNEMONIC)

        val paymentAmount = Value.lovelace(5_000_000L)
        val ctx = BuilderContext(
          environment.protocolParams,
          environment.evaluator,
          environment.network,
          backendService = backendService
        )
        val tx = ctx.buildNewTx
            .withPayer(account0.address)
            .payTo(account1.address, paymentAmount)
            .buildAndSign(account0.signer())

        submitTransactionToCardano(ctx, tx)

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
        val ctx = BuilderContext(
          environment.protocolParams,
          environment.evaluator,
          environment.network,
          backendService = backendService
        )

        val tx1 = ctx.buildNewTx
            .withPayer(account1.address)
            .payTo(scriptAddress, paymentAmount)
            .buildAndSign(account1.signer())

        println("Transferring to native script...")
        submitTransactionToCardano(ctx, tx1)
        println("Success!")
        println("Transferring from native script...")

        Thread.sleep(1_000)

        val tx2 = ctx.buildNewTx
            .withPayer(scriptAddress)
            .withAttachedNativeScript(nativeScript)
            .payTo(account2.address, Value.lovelace(3_000_000L))
            .buildAndSign(account0.signer())

        submitTransactionToCardano(ctx, tx2)
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
            .withPayer(account0.address)
            .withAttachedNativeScript(nativeScript)
            .mint(tokens, account3.address)
            .buildAndSign(account0.signer())

        submitTransactionToCardano(context, tx)
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

                val outs: scalus.prelude.SortedMap[
                  CurrencySymbol,
                  scalus.prelude.SortedMap[TokenName, BigInt]
                ] =
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
            .withPayer(account3.address)
            .payTo(scriptAddress, paymentAmount)
            .buildAndSign(account3.signer())

        println("Transferring to script...")
        submitTransactionToCardano(context, tx1)
        println("Success!")
        println("Transferring from script...")
        Thread.sleep(1_000)
        val tx2 = context.buildNewTx
            .withPayer(scriptAddress)
            .withAttachedScript(script)
            .payTo(account4.address, Value.lovelace(5_000_002L))
            .setCollateralPayer(account3.address)
            .buildAndSign(account3.signer()) // to be able to spend collaterals
        submitTransactionToCardano(context, tx2)
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
            .withPayer(account4.address)
            .withAttachedScript(plutusScript)
            .mint(tokens, account5.address)
            .setCollateralPayer(account4.address)
            .buildAndSign(account4.signer())

        submitTransactionToCardano(context, tx)
        println("Success!")
    }

    test("register stake address") {
        val network = Networks.testnet()
        val account = new Account(network, MNEMONIC)

        // Create a stake address from the same account
        val stakeKeyHash = AddrKeyHash(
          platform.blake2b_224(
            ByteString.fromArray(account.stakeHdKeyPair().getPublicKey.getKeyData)
          )
        )
        val stakeAddress =
            StakeAddress(Network.Testnet, StakePayload.fromBytes(stakeKeyHash.bytes, false).get)

        val context = BuilderContext(
          environment.protocolParams,
          environment.evaluator,
          environment.network,
          backendService = backendService
        )

        println("Registering stake address...")
        val tx =
            StakingTransactionBuilder(context, account5.address, stakeAddress).registerStakeAddress

        val signed = account5.signer().signTx(tx)
        submitTransactionToCardano(context, signed)
        println("Success!")
    }

    test("withdraw staking rewards") {
        val network = Networks.testnet()
        val account = new Account(
          network,
          MNEMONIC,
          DerivationPath.createExternalAddressDerivationPathForAccount(6)
        )

        // Create a stake address from the same account
        val stakeKeyHash = AddrKeyHash(
          platform.blake2b_224(
            ByteString.fromArray(account.stakeHdKeyPair().getPublicKey.getKeyData)
          )
        )
        val stakeAddress =
            StakeAddress(Network.Testnet, StakePayload.fromBytes(stakeKeyHash.bytes, false).get)

        val context = BuilderContext(
          environment.protocolParams,
          environment.evaluator,
          environment.network,
          backendService = backendService
        )

        println("Prepare stake addresss...")
        prepareStakeAddress(account)

        println("Withdrawing staking rewards...")
        val tx = StakingTransactionBuilder(context, account6.address, stakeAddress)
            .withdraw(0)

        val signed = account6.signer(a => Seq(a.hdKeyPair(), a.stakeHdKeyPair())).signTx(tx)
        submitTransactionToCardano(context, signed)
        println("Success!")
    }

    private def prepareStakeAddress(account: Account) = {
        QuickTxBuilder(backendService)
            .compose {
                new Tx()
                    .registerStakeAddress(account.stakeAddress())
                    .delegateVotingPowerTo(
                      account.stakeAddress(),
                      com.bloxbean.cardano.client.transaction.spec.governance.DRep.abstain()
                    )
                    .from(account.baseAddress())
            }
            .validFrom(0)
            .feePayer(account.baseAddress())
            .withSigner(SignerProviders.signerFrom(account.stakeHdKeyPair()))
            .withSigner(account.sign)
            .completeAndWait()
    }

    test("register a DRep") {
        // Use your own for the test
        val mnemonic =
            "burger sheriff ginger public attract machine loyal cluster armed guitar midnight fabric total update mixture all humble age spirit pottery wealth cherry fork embark"
        val BLOCKFROST_PROJECT_ID = "preprod9cHsFFm9r39J6L9BCcrCCZAUk56GXMMY"
        val blockfrostUrl = "https://cardano-preprod.blockfrost.io/api/v0/"
        val backend = new BFBackendService(blockfrostUrl, BLOCKFROST_PROJECT_ID)
        val account = new Account(Networks.preprod(), mnemonic)

        val context = BuilderContext(
          environment.protocolParams,
          environment.evaluator,
          environment.network,
          backendService = backend
        )

        println("Registering DRep...")
        val tx = context.buildNewTx.registerDrep(account).buildAndSign()
        submitTransactionToCardano(context, tx)
        Thread.sleep(40_000)
        println("Success!")
    }

    test("unregister a DRep") {
        // Use your own for the test
        val mnemonic =
            "burger sheriff ginger public attract machine loyal cluster armed guitar midnight fabric total update mixture all humble age spirit pottery wealth cherry fork embark"
        val BLOCKFROST_PROJECT_ID = "preprod9cHsFFm9r39J6L9BCcrCCZAUk56GXMMY"
        val blockfrostUrl = "https://cardano-preprod.blockfrost.io/api/v0/"
        val backend = new BFBackendService(blockfrostUrl, BLOCKFROST_PROJECT_ID)
        val account = new Account(Networks.preprod, mnemonic)

        val context = BuilderContext(
          environment.protocolParams,
          environment.evaluator,
          environment.network,
          backendService = backend
        )

        val senderAddress = Address.fromBech32(account.baseAddress())
        println("Unregistering DRep...")
        val tx = context.buildNewTx.unRegistrerDrep(account).buildAndSign()
        println(tx.body.value.inputs)
        submitTransactionToCardano(context, tx)
        println("Success!")
    }

    def makeSignerFrom(derivation: String, mnemonic: String)(
        keyPairToUse: Account => Seq[HdKeyPair] = a => Seq(a.hdKeyPair())
    ) = {
        val derivationPieces = derivation.split("/").drop(1).map(_.stripSuffix("'")).map(_.toInt)

        val derivationPath = DerivationPath
            .builder()
            .purpose(new Segment(derivationPieces(0), true))
            .coinType(new Segment(derivationPieces(1), true))
            .account(new Segment(derivationPieces(2), true))
            .role(new Segment(derivationPieces(3), false))
            .index(new Segment(derivationPieces(4), false))
            .build()
        val account = new Account(Networks.testnet(), mnemonic, derivationPath)
        val pairs = keyPairToUse(account)
        new TxSigner {
            override def signTx(unsigned: Transaction): Transaction = {
                val signatures = pairs.map { hdKeyPair =>
                    val privateK = hdKeyPair.getPrivateKey
                    val publicK = hdKeyPair.getPublicKey
                    val signature =
                        signEd25519(privateK.getKeyData, publicK.getKeyData, unsigned.id.bytes)
                    (privateK, publicK, signature)
                }

                val VKeyWitnesses = signatures.map { (priv, pub, sign) =>
                    VKeyWitness(
                      ByteString.fromArray(pub.getKeyData),
                      ByteString.fromArray(sign)
                    )
                }.toSet

                val ws = unsigned.witnessSet.copy(vkeyWitnesses = VKeyWitnesses)
                unsigned.copy(witnessSet = ws)
            }
        }
    }

    /*
     * This method implements slip-001 ed25519 signatures, which is the way to sign transactions on
     * cardano, hence why we cannot just use the public bouncycastle ed25519 API.
     *
     * In an ideal world, we use the public API, for which we need bouncycastle to expose this method.
     */
    def signEd25519(
        cardanoExtendedPrivKey: Array[Byte],
        publicKey: Array[Byte],
        data: Array[Byte]
    ): Array[Byte] = {
        // private static Unit implSign(d: Digest, h: Array[Byte], s: Array[Byte], pk: Array[Byte], pkOff: Int, ctx: Array[Byte], phflag: Byte, m: Array[Byte], mOff: Int, mLen: Int, sig: Array[Byte], sigOff: Int)
        val ed25519Class = classOf[org.bouncycastle.math.ec.rfc8032.Ed25519]
        val method = ed25519Class.getDeclaredMethod(
          "implSign",
          classOf[org.bouncycastle.crypto.Digest],
          classOf[Array[Byte]],
          classOf[Array[Byte]],
          classOf[Array[Byte]],
          classOf[Int],
          classOf[Array[Byte]],
          classOf[Byte],
          classOf[Array[Byte]],
          classOf[Int],
          classOf[Int],
          classOf[Array[Byte]],
          classOf[Int]
        )
        method.setAccessible(true)
        val d = new SHA512Digest
        val h = new Array[Byte](64)
        Array.copy(cardanoExtendedPrivKey, 0, h, 0, 64)
        val s = new Array[Byte](32)
        Array.copy(cardanoExtendedPrivKey, 0, s, 0, 32)
        val pk = new Array[Byte](32)
        Array.copy(publicKey, 0, pk, 0, 32)
        val ctx = null
        val phflag: Byte = 0
        val m = data
        val mOff = 0
        val mLen = data.length
        val sig = new Array[Byte](64)
        val sigOff = 0
        method.invoke(null, d, h, s, pk, 0, ctx, phflag, m, mOff, mLen, sig, sigOff)
        sig
    }
}
