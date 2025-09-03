package scalus.bloxbean

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.crypto.bip32.HdKeyPair
import com.bloxbean.cardano.client.crypto.cip1852.DerivationPath
import com.bloxbean.cardano.client.crypto.cip1852.DerivationPath.createExternalAddressDerivationPath
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString
import scalus.cardano.address.*
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.{CardanoMutator, Context, State, UtxoEnv}
import scalus.cardano.ledger.txbuilder.*
import scalus.ledger.api.MajorProtocolVersion
import scalus.ledger.babbage.ProtocolParams
import scalus.uplc.eval.ExBudget

import java.security.MessageDigest

def resolvedTransaction(txHash: String, address: Address) = {
    TransactionInput(
      TransactionHash.fromHex(txHash),
      0
    ) -> TransactionOutput(address, Value.lovelace(1_000_000_000_000L))
}

class YaciDevkitIntegrationTest extends AnyFunSuite {

    private val EXISTING_UTXO = "6d36c0e2f304a5c27b85b3f04e95fc015566d35aef5f061c17c70e3e8b9ee508"
    private val SPENDER_ADDRESS =
        "addr_test1qryvgass5dsrf2kxl3vgfz76uhp83kv5lagzcp29tcana68ca5aqa6swlq6llfamln09tal7n5kvt4275ckwedpt4v7q48uhex"
    private val SPENDER_PAYMENT_KEY =
        "ed25519e_sk1sqr5ymxr5377q2tww7qzj8wdf9uwsx530p6txpfktvdsjvh2t3dk3q27c7gkel6anmfy4a2g6txy0f4mquwmj3pppvy3046006ulussa20jpu"
    private val TARGET_ADDRESS =
        "addr_test1qpqy3lufef8c3en9nrnzp2svwy5vy9zangvp46dy4qw23clgfxhn3pqv243d6wptud7fuaj5tjqer7wc7m036gx0emsqaqa8te"
    private val MNEMONIC =
        "test test test test test test test test test test test test test test test test test test test test test test test sauce"

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
        if !result.isSuccessful then {
            throw new RuntimeException(result.getResponse)
        }
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

        val spenderAddress = Address.fromBech32(SPENDER_ADDRESS)

        val utxo = List(resolvedTransaction(EXISTING_UTXO, spenderAddress)).toMap

        val inputSelector = InputSelector(
          Set(ResolvedTxInput.Pubkey(utxo.head)),
          collateral = Set.empty
        )

        val changeReturnStrategy = ChangeReturnStrategy.toAddress(spenderAddress)
        val feePayerStrategy = FeePayerStrategy.subtractFromAddress(spenderAddress)

        val interpreter = InterpreterWithProvidedData(
          inputSelector = inputSelector,
          utxo = utxo,
          environment = environment,
          changeReturnStrategy = changeReturnStrategy,
          feePayerStrategy = feePayerStrategy,
          evaluator = environment.evaluator
        )

        val recipientAddress = Address.fromBech32(TARGET_ADDRESS)
        val paymentIntention = Intention.Pay(
          address = recipientAddress,
          value = Value(Coin(2_000_000L))
        )

        val tx = interpreter.realize(paymentIntention)

        // Compare with our derived public key hash
        val (privateKeyBytes, publicKeyBytes) = keyPairUsingDerivation
        val privateKey: ByteString = ByteString.fromArray(privateKeyBytes)
        val publicKey: ByteString = ByteString.fromArray(publicKeyBytes)

        val signed = TxSigner
            .usingKeyPairs(publicKey -> privateKey)
            .signTx(tx)

        val cborBytes = signed.toCbor
        val ourLedgerRulesVerificationResult = CardanoMutator(
          Context(signed.body.value.fee, UtxoEnv(0L, params, CertState.empty)),
          State(utxo, CertState.empty),
          signed
        )
        println(ourLedgerRulesVerificationResult)

        submitTransactionToCardano(cborBytes)
    }

    def keyPairUsingDerivation: (Array[Byte], Array[Byte]) = {
        // Take the first 32 bytes of XPrv → call this seed
        val xprv: Array[Byte] = keyPairUsingBloxbean.getPrivateKey.getKeyData
        val seed = xprv.take(32)

        // Compute the expanded private key: Hash seed with SHA-512 → 64 bytes
        val sha512 = MessageDigest.getInstance("SHA-512")
        val expandedKey = sha512.digest(seed)

        // Clamp the first 32 bytes to get the private scalar
        val privateScalar = expandedKey.take(32)

        // Ed25519 clamping operations:
        // Clear the lowest 3 bits of byte 0
        privateScalar(0) = (privateScalar(0) & 248).toByte // 0b11111000

        // Clear the highest 2 bits of byte 31
        privateScalar(31) = (privateScalar(31) & 63).toByte // 0b00111111

        // Set the second highest bit of byte 31
        privateScalar(31) = (privateScalar(31) | 64).toByte // 0b01000000

        val publicKey = xprv.slice(32, 64)

        (privateScalar, publicKey)
    }

    def keyPairUsingBloxbean: HdKeyPair = {
        import com.bloxbean.cardano.client.common.model.Network as Nw
        val derivationPath = createExternalAddressDerivationPath
        val acc = Account(new Nw(0, 42), MNEMONIC, derivationPath)
        acc.hdKeyPair()
    }
}
