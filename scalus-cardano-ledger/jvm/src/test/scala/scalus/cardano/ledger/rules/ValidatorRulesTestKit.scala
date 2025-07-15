package scalus.cardano.ledger.rules

import org.scalacheck.Arbitrary
import scalus.cardano.ledger.*
import scalus.builtin.ByteString
import java.security.SecureRandom
import org.bouncycastle.crypto.generators.Ed25519KeyPairGenerator
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.params.{Ed25519KeyGenerationParameters, Ed25519PrivateKeyParameters, Ed25519PublicKeyParameters}

trait ValidatorRulesTestKit extends ArbitraryInstances {
    protected def randomValidTransaction: Transaction =
        Arbitrary.arbitrary[Transaction].sample.get.copy(isValid = true)

    private val keyPairGenerator = {
        val keyPairGenerator = new Ed25519KeyPairGenerator()
        keyPairGenerator.init(new Ed25519KeyGenerationParameters(new SecureRandom()))
        keyPairGenerator
    }

    protected def generateKeyPair(): (ByteString, ByteString) = {
        val asymmetricCipherKeyPair: AsymmetricCipherKeyPair = keyPairGenerator.generateKeyPair()
        val privateKeyParams: Ed25519PrivateKeyParameters =
            asymmetricCipherKeyPair.getPrivate.asInstanceOf[Ed25519PrivateKeyParameters];
        val publicKeyParams: Ed25519PublicKeyParameters =
            asymmetricCipherKeyPair.getPublic.asInstanceOf[Ed25519PublicKeyParameters];
        val privateKey: ByteString = ByteString.fromArray(privateKeyParams.getEncoded)
        val publicKey: ByteString = ByteString.fromArray(publicKeyParams.getEncoded)
        (privateKey, publicKey)
    }
}
