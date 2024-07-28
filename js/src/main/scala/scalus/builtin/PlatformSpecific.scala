package scalus.builtin

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.scalajs.js.typedarray.Int8Array
import scala.scalajs.js.typedarray.Uint8Array
import js.JSConverters.*

@JSImport("crypto", JSImport.Namespace)
@js.native
private object Crypto extends js.Object {
    def createHash(algorithm: String): Hash = js.native
}

@JSImport("@noble/hashes/blake2b", "blake2b")
@js.native
private object Blake2b extends js.Object {
    def create(opts: BlakeOpts): Hash = js.native
}

private type Input = Uint8Array | Null

private class BlakeOpts(val dkLen: Int) extends js.Object

@js.native
private trait Hash extends js.Object {
    def update(data: Uint8Array): Hash = js.native
    def digest(): Uint8Array = js.native
}

@JSImport("@noble/curves/secp256k1", JSImport.Namespace)
@js.native
private object Secp256k1 extends js.Object {
    def ecdsaVerify(message: Uint8Array, signature: Uint8Array, publicKey: Uint8Array): Boolean =
        js.native
    def schnorrVerify(message: Uint8Array, signature: Uint8Array, publicKey: Uint8Array): Boolean =
        js.native
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

trait NodeJsPlatformSpecific extends PlatformSpecific {
    extension (bs: ByteString)
        def toUint8Array: Uint8Array =
            val int8Array = new Int8Array(bs.bytes.toJSArray)
            new Uint8Array(int8Array.buffer, int8Array.byteOffset, int8Array.length)

    extension (arr: Uint8Array)
        def toByteString: ByteString =
            ByteString.fromArray(new Int8Array(arr.buffer, arr.byteOffset, arr.length).toArray)

    override def sha2_256(bs: ByteString): ByteString =
        val hash = Crypto.createHash("sha256")
        hash.update(bs.toUint8Array).digest().toByteString

    override def sha3_256(bs: ByteString): ByteString =
        val hash = Crypto.createHash("sha3-256")
        hash.update(bs.toUint8Array).digest().toByteString

    override def blake2b_224(bs: ByteString): ByteString =
        val hash = Blake2b.create(BlakeOpts(dkLen = 28))
        hash.update(bs.toUint8Array).digest().toByteString

    override def blake2b_256(bs: ByteString): ByteString =
        val hash = Blake2b.create(BlakeOpts(dkLen = 32))
        hash.update(bs.toUint8Array).digest().toByteString

    override def verifyEd25519Signature(pk: ByteString, msg: ByteString, sig: ByteString): Boolean =
        if pk.length != 32 then
            throw new IllegalArgumentException(s"Invalid public key length ${pk.length}")
        if sig.length != 64 then
            throw new IllegalArgumentException(s"Invalid signature length ${sig.length}")
        Ed25519Curves.ed25519.verify(sig.toUint8Array, msg.toUint8Array, pk.toUint8Array)

    override def verifyEcdsaSecp256k1Signature(
        pk: ByteString,
        msg: ByteString,
        sig: ByteString
    ): Boolean =
        Secp256k1.ecdsaVerify(msg.toUint8Array, sig.toUint8Array, pk.toUint8Array)

    override def verifySchnorrSecp256k1Signature(
        pk: ByteString,
        msg: ByteString,
        sig: ByteString
    ): Boolean =
        Secp256k1.schnorrVerify(msg.toUint8Array, sig.toUint8Array, pk.toUint8Array)
}

object NodeJsPlatformSpecific extends NodeJsPlatformSpecific

given PlatformSpecific = NodeJsPlatformSpecific
