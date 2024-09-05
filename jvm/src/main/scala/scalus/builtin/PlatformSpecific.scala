package scalus.builtin

import org.bitcoins.crypto.ECDigitalSignature
import org.bitcoins.crypto.ECPublicKey
import org.bitcoins.crypto.SchnorrDigitalSignature
import org.bitcoins.crypto.SchnorrPublicKey
import org.bouncycastle.crypto.digests.Blake2bDigest
import org.bouncycastle.crypto.params.Ed25519PublicKeyParameters
import org.bouncycastle.crypto.signers.Ed25519Signer
import org.bouncycastle.jcajce.provider.digest.SHA3
import org.bouncycastle.jcajce.provider.digest.Keccak
import scalus.utils.Utils
import scodec.bits.ByteVector
import supranational.blst

object JVMPlatformSpecific extends JVMPlatformSpecific
trait JVMPlatformSpecific extends PlatformSpecific {
    override def sha2_256(bs: ByteString): ByteString =
        ByteString.unsafeFromArray(Utils.sha2_256(bs.bytes))

    override def sha3_256(bs: ByteString): ByteString =
        val digestSHA3 = new SHA3.Digest256()
        ByteString.unsafeFromArray(digestSHA3.digest(bs.bytes))

    override def blake2b_224(bs: ByteString): ByteString =
        val digest = new Blake2bDigest(224)
        digest.update(bs.bytes, 0, bs.length)
        val hash = new Array[Byte](digest.getDigestSize)
        digest.doFinal(hash, 0)
        ByteString.unsafeFromArray(hash)

    override def blake2b_256(bs: ByteString): ByteString =
        val digest = new Blake2bDigest(256)
        digest.update(bs.bytes, 0, bs.length)
        val hash = new Array[Byte](digest.getDigestSize)
        digest.doFinal(hash, 0)
        ByteString.unsafeFromArray(hash)

    override def verifySchnorrSecp256k1Signature(
        pk: ByteString,
        msg: ByteString,
        sig: ByteString
    ): Boolean = {
        if pk.length != 32 then
            throw new IllegalArgumentException(s"Invalid public key length ${pk.length}")
        if sig.length != 64 then
            throw new IllegalArgumentException(s"Invalid signature length ${sig.length}")
        val signature = SchnorrDigitalSignature(ByteVector(sig.bytes))
        SchnorrPublicKey(ByteVector(pk.bytes)).verify(ByteVector(msg.bytes), signature)
    }

    override def verifyEd25519Signature(pk: ByteString, msg: ByteString, sig: ByteString): Boolean =
        if pk.length != 32 then
            throw new IllegalArgumentException(s"Invalid public key length ${pk.length}")
        if sig.length != 64 then
            throw new IllegalArgumentException(s"Invalid signature length ${sig.length}")
        val pubKeyParams =
            try new Ed25519PublicKeyParameters(pk.bytes, 0)
            catch
                case e: IllegalArgumentException =>
                    return false
        val verifier = new Ed25519Signer()
        verifier.init(false, pubKeyParams)
        verifier.update(msg.bytes, 0, msg.length)
        verifier.verifySignature(sig.bytes)

    override def verifyEcdsaSecp256k1Signature(
        pk: ByteString,
        msg: ByteString,
        sig: ByteString
    ): Boolean =
        if pk.length != 33 then
            throw new IllegalArgumentException(s"Invalid public key length ${pk.length}")
        if msg.length != 32 then
            throw new IllegalArgumentException(s"Invalid message length ${msg.length}")
        if sig.length != 64 then
            throw new IllegalArgumentException(s"Invalid signature length ${sig.length}")
        val signature = ECDigitalSignature.fromRS(ByteVector(sig.bytes))
        ECPublicKey(ByteVector(pk.bytes)).verify(ByteVector(msg.bytes), signature)

    // BLS12_381 operations
    override def bls12_381_G1_equal(p1: BLS12_381_G1_Element, p2: BLS12_381_G1_Element): Boolean =
        ???

    override def bls12_381_G1_add(
        p1: BLS12_381_G1_Element,
        p2: BLS12_381_G1_Element
    ): BLS12_381_G1_Element =
        ???

    override def bls12_381_G1_scalarMul(s: BigInt, p: BLS12_381_G1_Element): BLS12_381_G1_Element =
        ???

    override def bls12_381_G1_neg(
        p: BLS12_381_G1_Element
    ): BLS12_381_G1_Element = ???

    /** Compress a G1 element to a bytestring. This serialises a curve point to its x coordinate
      * only. The compressed bytestring is 48 bytes long, with three spare bits used to convey extra
      * information about the point, including determining which of two possible y coordinates the
      * point has and whether the point is the point at infinity.
      * @see
      *   https://github.com/supranational/blst#serialization-format
      *
      * @param p
      *   G1 element to compress
      * @return
      *   Compressed bytestring
      */
    override def bls12_381_G1_compress(p: BLS12_381_G1_Element): ByteString = {
        val compressed = new Array[Byte](48)
        ByteString.fromArray(p.p.compress())
    }

    override def bls12_381_G1_uncompress(bs: ByteString): BLS12_381_G1_Element = {
        val p = blst.P1.uncompress(bs.bytes)
        BLS12_381_G1_Element(p.to_jacobian())
    }

    override def bls12_381_G1_hashToGroup(bs: ByteString, dst: ByteString): BLS12_381_G1_Element =
        ???

    override def bls12_381_G1_compressed_zero: ByteString = ???

    override def bls12_381_G1_compressed_generator: ByteString = ???

    override def bls12_381_G2_equal(p1: BLS12_381_G2_Element, p2: BLS12_381_G2_Element): Boolean =
        ???

    override def bls12_381_G2_add(
        p1: BLS12_381_G2_Element,
        p2: BLS12_381_G2_Element
    ): BLS12_381_G2_Element =
        ???

    override def bls12_381_G2_scalarMul(s: BigInt, p: BLS12_381_G2_Element): BLS12_381_G2_Element =
        ???

    override def bls12_381_G2_neg(
        p: BLS12_381_G2_Element
    ): BLS12_381_G2_Element = ???

    override def bls12_381_G2_compress(
        p: BLS12_381_G2_Element
    ): ByteString = ???
    override def bls12_381_G2_uncompress(
        bs: ByteString
    ): BLS12_381_G2_Element =
        ???
    override def bls12_381_G2_hashToGroup(bs: ByteString, dst: ByteString): BLS12_381_G2_Element =
        ???

    override def bls12_381_G2_compressed_zero: ByteString = ???

    override def bls12_381_G2_compressed_generator: ByteString = ???

    override def bls12_381_millerLoop(
        p1: BLS12_381_G1_Element,
        p2: BLS12_381_G2_Element
    ): BLS12_381_MlResult =
        ???

    override def bls12_381_mulMlResult(
        r1: BLS12_381_MlResult,
        r2: BLS12_381_MlResult
    ): BLS12_381_MlResult =
        ???

    override def bls12_381_finalVerify(r: BLS12_381_MlResult): Boolean =
        ???

    override def keccak_256(bs: ByteString): ByteString = {
        val digest = new Keccak.Digest256()
        ByteString.unsafeFromArray(digest.digest(bs.bytes))
    }
}

given PlatformSpecific = JVMPlatformSpecific
