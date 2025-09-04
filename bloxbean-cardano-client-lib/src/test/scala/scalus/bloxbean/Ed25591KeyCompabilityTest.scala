package scalus.bloxbean

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.{Builtins, ByteString, JVMPlatformSpecific}
import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.common.model.Networks
import org.bouncycastle.crypto.Digest
import org.bouncycastle.crypto.digests.SHA512Digest
import org.bouncycastle.crypto.params.Ed25519PrivateKeyParameters
import org.bouncycastle.crypto.params.Ed25519PublicKeyParameters
import org.bouncycastle.crypto.signers.Ed25519Signer

class Ed25591KeyCompabilityTest extends AnyFunSuite {

    import org.bouncycastle.crypto.params.Ed25519PrivateKeyParameters
    import java.lang.reflect.Field

    def makeInternalCall(
        cardanoExtendedPrivKey: Array[Byte],
        publicKey: Array[Byte],
        data: Array[Byte]
    ): Array[Byte] = {
        // .    private static Unit implSign(d: Digest, h: Array[Byte], s: Array[Byte], pk: Array[Byte], pkOff: Int, ctx: Array[Byte], phflag: Byte, m: Array[Byte], mOff: Int, mLen: Int, sig: Array[Byte], sigOff: Int)
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

    test("account shoule be generated from mnemonic ans aign bytes with Ed125591Key") {
        val mnemonic =
            "test test test test test test test test test test test test test test test test test test test test test test test sauce"
        val derivation = "m/1852'/1815'/0'/0/0"
        val account = new Account(Networks.testnet, mnemonic)
        val keyPair = account.hdKeyPair()
        val publicKeyData = keyPair.getPublicKey().getKeyData()

        // val keySeed = keyPair.getPrivateKey().getKeyData().take(32)
        val privateKeyData = keyPair.getPrivateKey().getKeyData()
        val keySeedFE = privateKeyData.take(64)

        val testData = "hello world".getBytes()

        val signature3 = makeInternalCall(privateKeyData, publicKeyData, testData)

        val verifier = new Ed25519Signer()
        verifier.init(false, new Ed25519PublicKeyParameters(publicKeyData, 0))
        verifier.update(testData, 0, testData.length)
        val isValid = verifier.verifySignature(signature3)

        // println(s"verified - bouncycastle: ${isValid}")
        assert(isValid)
    }

}
