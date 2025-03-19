package scalus.builtin

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.*
import scala.scalajs.js.typedarray.Int8Array
import scala.scalajs.js.typedarray.Uint8Array

@JSImport("@noble/hashes/sha2", JSImport.Namespace)
@js.native
private object Sha2 extends js.Object {
    def sha256(msg: Uint8Array): Uint8Array = js.native
}

@JSImport("@noble/hashes/sha3", JSImport.Namespace)
@js.native
private object Sha3 extends js.Object {
    def sha3_256(msg: Uint8Array): Uint8Array = js.native
    def keccak_256(msg: Uint8Array): Uint8Array = js.native
}

@JSImport("@noble/hashes/blake2b", "blake2b")
@js.native
private object Blake2b extends js.Object {
    def create(opts: BlakeOpts): Hash = js.native
}

private class BlakeOpts(val dkLen: Int) extends js.Object

@js.native
private trait Hash extends js.Object {
    def update(data: Uint8Array): Hash = js.native
    def digest(): Uint8Array = js.native
}

@JSImport("@noble/hashes/ripemd160", JSImport.Namespace)
@js.native
private object Ripemd160 extends js.Object {
    def ripemd160(msg: Uint8Array): Uint8Array = js.native
}

@JSImport("@noble/curves/secp256k1", JSImport.Namespace)
@js.native
private object Secp256k1Curve extends js.Object {
    val secp256k1: Secp256k1 = js.native
    val schnorr: Secp256k1Schnorr = js.native
}

@JSImport("@noble/curves/ed25519", JSImport.Namespace)
@js.native
private object Ed25519Curves extends js.Object {
    val ed25519: Ed25519 = js.native
}

@js.native
private trait Ed25519 extends js.Object {
    def verify(signature: Uint8Array, message: Uint8Array, publicKey: Uint8Array): Boolean =
        js.native
}

@js.native
private trait Secp256k1 extends js.Object {
    def verify(signature: Uint8Array, message: Uint8Array, publicKey: Uint8Array): Boolean =
        js.native

    def ProjectivePoint: ProjectivePointModule = js.native
}

@js.native
trait ProjectivePointModule extends js.Object:
    def fromHex(bytes: Uint8Array): ProjectivePoint = js.native

@js.native
trait ProjectivePoint extends js.Object:
    def toAffine(): js.Object = js.native

@js.native
private trait Secp256k1Schnorr extends js.Object {
    def verify(signature: Uint8Array, message: Uint8Array, publicKey: Uint8Array): Boolean =
        js.native
}

trait NodeJsPlatformSpecific extends PlatformSpecific {
    extension (bs: ByteString)
        def toUint8Array: Uint8Array =
            val int8Array = new Int8Array(bs.bytes.toJSArray)
            new Uint8Array(int8Array.buffer, int8Array.byteOffset, int8Array.length)

    extension (arr: Uint8Array)
        def toByteString: ByteString =
            ByteString.unsafeFromArray(
              new Int8Array(arr.buffer, arr.byteOffset, arr.length).toArray
            )

    extension (bigInt: BigInt) def toJsBigInt: js.BigInt = js.BigInt(bigInt.toString())

    override def sha2_256(bs: ByteString): ByteString =
        Sha2.sha256(bs.toUint8Array).toByteString

    override def sha3_256(bs: ByteString): ByteString =
        Sha3.sha3_256(bs.toUint8Array).toByteString

    override def blake2b_224(bs: ByteString): ByteString =
        val hash = Blake2b.create(BlakeOpts(dkLen = 28))
        hash.update(bs.toUint8Array).digest().toByteString

    override def blake2b_256(bs: ByteString): ByteString =
        val hash = Blake2b.create(BlakeOpts(dkLen = 32))
        hash.update(bs.toUint8Array).digest().toByteString

    override def verifyEd25519Signature(pk: ByteString, msg: ByteString, sig: ByteString): Boolean =
        require(pk.length == 32, s"Invalid public key length ${pk.length}")
        require(sig.length == 64, s"Invalid signature length ${sig.length}")
        Ed25519Curves.ed25519.verify(sig.toUint8Array, msg.toUint8Array, pk.toUint8Array)

    private def isValidPublicKey(pubKey: ByteString): Boolean =
        try
            val point = Secp256k1Curve.secp256k1.ProjectivePoint.fromHex(
              pubKey.toUint8Array
            ) // Use fromBytes instead of fromHex
            point.toAffine() // Ensures key is valid
            true
        catch case e: Throwable => false

    override def verifyEcdsaSecp256k1Signature(
        pk: ByteString,
        msg: ByteString,
        sig: ByteString
    ): Boolean = {
        require(pk.length == 33, s"Invalid public key length ${pk.length}, expected 33")
        require(isValidPublicKey(pk), s"Invalid public key ${pk}")
        require(msg.length == 32, s"Invalid message length ${msg.length}, expected 32")
        require(sig.length == 64, s"Invalid signature length ${sig.length}, expected 64")

        Secp256k1Curve.secp256k1.verify(sig.toUint8Array, msg.toUint8Array, pk.toUint8Array)
    }

    override def verifySchnorrSecp256k1Signature(
        pk: ByteString,
        msg: ByteString,
        sig: ByteString
    ): Boolean =
        require(pk.length == 32, s"Invalid public key length ${pk.length}, expected 33")
        require(
          isValidPublicKey(ByteString.fromArray(0x02 +: pk.bytes)),
          s"Invalid public key ${pk}"
        )
        require(msg.length == 32, s"Invalid message length ${msg.length}, expected 32")
        require(sig.length == 64, s"Invalid signature length ${sig.length}, expected 64")
        Secp256k1Curve.schnorr.verify(sig.toUint8Array, msg.toUint8Array, pk.toUint8Array)

    // BLS12_381 operations
    override def bls12_381_G1_equal(
        elem1: BLS12_381_G1_Element,
        elem2: BLS12_381_G1_Element
    ): Boolean = elem1 == elem2

    override def bls12_381_G1_add(
        elem1: BLS12_381_G1_Element,
        elem2: BLS12_381_G1_Element
    ): BLS12_381_G1_Element = elem1 + elem2

    override def bls12_381_G1_scalarMul(
        scalar: BigInt,
        elem: BLS12_381_G1_Element
    ): BLS12_381_G1_Element = elem * scalar

    override def bls12_381_G1_neg(elem: BLS12_381_G1_Element): BLS12_381_G1_Element = -elem

    override def bls12_381_G1_compress(elem: BLS12_381_G1_Element): ByteString =
        elem.toCompressedByteString

    override def bls12_381_G1_uncompress(byteString: ByteString): BLS12_381_G1_Element =
        BLS12_381_G1_Element.fromCompressedByteString(byteString)

    override def bls12_381_G1_hashToGroup(
        byteString: ByteString,
        dst: ByteString
    ): BLS12_381_G1_Element = BLS12_381_G1_Element.hashToGroup(byteString, dst)

    override def bls12_381_G2_equal(
        elem1: BLS12_381_G2_Element,
        elem2: BLS12_381_G2_Element
    ): Boolean = elem1 == elem2

    override def bls12_381_G2_add(
        elem1: BLS12_381_G2_Element,
        elem2: BLS12_381_G2_Element
    ): BLS12_381_G2_Element = elem1 + elem2

    override def bls12_381_G2_scalarMul(
        scalar: BigInt,
        elem: BLS12_381_G2_Element
    ): BLS12_381_G2_Element = elem * scalar

    override def bls12_381_G2_neg(
        elem: BLS12_381_G2_Element
    ): BLS12_381_G2_Element = -elem

    override def bls12_381_G2_compress(elem: BLS12_381_G2_Element): ByteString =
        elem.toCompressedByteString

    override def bls12_381_G2_uncompress(byteString: ByteString): BLS12_381_G2_Element =
        BLS12_381_G2_Element.fromCompressedByteString(byteString)

    override def bls12_381_G2_hashToGroup(
        byteString: ByteString,
        dst: ByteString
    ): BLS12_381_G2_Element = BLS12_381_G2_Element.hashToGroup(byteString, dst)

    override def bls12_381_millerLoop(
        elemG1: BLS12_381_G1_Element,
        elemG2: BLS12_381_G2_Element
    ): BLS12_381_MlResult =
        BLS12_381_MlResult(elemG1, elemG2)

    override def bls12_381_mulMlResult(
        lhs: BLS12_381_MlResult,
        rhs: BLS12_381_MlResult
    ): BLS12_381_MlResult =
        lhs * rhs

    override def bls12_381_finalVerify(lhs: BLS12_381_MlResult, rhs: BLS12_381_MlResult): Boolean =
        lhs == rhs

    override def keccak_256(bs: ByteString): ByteString =
        Sha3.keccak_256(bs.toUint8Array).toByteString

    override def ripemd_160(byteString: ByteString): ByteString =
        Ripemd160.ripemd160(byteString.toUint8Array).toByteString
}

object NodeJsPlatformSpecific extends NodeJsPlatformSpecific

given PlatformSpecific = NodeJsPlatformSpecific
