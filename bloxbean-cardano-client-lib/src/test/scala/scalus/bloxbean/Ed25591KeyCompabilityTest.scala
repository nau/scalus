package scalus.bloxbean

import org.scalatest.funsuite.AnyFunSuite
import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.common.model.Networks
import com.bloxbean.cardano.client.crypto.cip1852.DerivationPath
import org.bouncycastle.crypto.params.Ed25519PublicKeyParameters
import org.bouncycastle.crypto.signers.Ed25519Signer

class Ed25591KeyCompabilityTest extends AnyFunSuite {

    test("account shoule be generated from mnemonic ans aign bytes with Ed125591Key") {
        val mnemonic =
            "test test test test test test test test test test test test test test test test test test test test test test test sauce"
        val derivation = "m/1852'/1815'/0'/0/0"
        val derivationPath = DerivationPath.createExternalAddressDerivationPath()
        val account = new Account(Networks.testnet, mnemonic, derivationPath)
        val keyPair = account.hdKeyPair()
        val publicKeyData = keyPair.getPublicKey().getKeyData()

        // val keySeed = keyPair.getPrivateKey().getKeyData().take(32)
        val privateKeyData = keyPair.getPrivateKey().getKeyData()
        val keySeedFE = privateKeyData.take(64)

        val testData = "hello world".getBytes()

        val signature3 = signEd25519(privateKeyData, publicKeyData, testData)

        val verifier = new Ed25519Signer()
        verifier.init(false, new Ed25519PublicKeyParameters(publicKeyData, 0))
        verifier.update(testData, 0, testData.length)
        val isValid = verifier.verifySignature(signature3)

        // println(s"verified - bouncycastle: ${isValid}")
        assert(isValid)
    }

}
