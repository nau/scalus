package scalus.builtin

import scala.scalanative.runtime
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

@link("sodium")
@extern
object LibSodium {
    def sodium_init(): Int = extern

    def crypto_hash_sha256_bytes(): CSize = extern

    def crypto_hash_sha256(out: Ptr[Byte], in: Ptr[Byte], inlen: CUnsignedLongLong): CInt = extern

    // SHA3-256
    def crypto_hash_sha3_256_bytes(): CSize = extern
    def crypto_hash_sha3_256(out: Ptr[Byte], in: Ptr[Byte], inlen: CUnsignedLongLong): CInt = extern

    // BLAKE2b with custom output length
    def crypto_generichash_blake2b_bytes_min(): CSize = extern
    def crypto_generichash_blake2b_bytes_max(): CSize = extern
    def crypto_generichash_blake2b(
        out: Ptr[Byte],
        outlen: CSize,
        in: Ptr[Byte],
        inlen: CUnsignedLongLong,
        key: Ptr[Byte],
        keylen: CSize
    ): CInt = extern

    def crypto_sign_ed25519_verify_detached(
        sig: Ptr[Byte],
        msg: Ptr[Byte],
        msglen: CUnsignedLongLong,
        key: Ptr[Byte]
    ): CInt = extern

}

object Sodium {
    import LibSodium.*
    private inline def hashWithSodium(
        input: Array[Byte],
        hashLen: Int,
        hashFunc: (Ptr[Byte], Ptr[Byte], CUnsignedLongLong) => CInt
    ): Array[Byte] =
        val output = new Array[Byte](hashLen)
        hashFunc(output.atUnsafe(0), input.atUnsafe(0), input.length.toULong)
        output

    def sha256(input: Array[Byte]): Array[Byte] =
        hashWithSodium(
          input,
          crypto_hash_sha256_bytes().toInt,
          LibSodium.crypto_hash_sha256
        )

    private def blake2b(
        input: Array[Byte],
        outputLength: Int,
        key: Array[Byte] = Array.empty
    ): Array[Byte] = {
        val output = new Array[Byte](outputLength)
        val keyPtr = if key.nonEmpty then key.atUnsafe(0) else null

        LibSodium.crypto_generichash_blake2b(
          output.atUnsafe(0),
          outputLength.toCSize,
          input.atUnsafe(0),
          input.length.toULong,
          keyPtr,
          key.length.toCSize
        )
        output
    }

    def blake2b224(input: Array[Byte], key: Array[Byte] = Array.empty): Array[Byte] =
        blake2b(input, 28, key) // 224 bits = 28 bytes

    def blake2b256(input: Array[Byte], key: Array[Byte] = Array.empty): Array[Byte] =
        blake2b(input, 32, key) // 256 bits = 32 bytes

    def verifyEd25519Signature(
        pubKey: Array[Byte],
        msg: Array[Byte],
        signature: Array[Byte]
    ): Boolean =
        if pubKey.length != 32 || signature.length != 64 then false
        else
            LibSodium.crypto_sign_ed25519_verify_detached(
              signature.atUnsafe(0),
              msg.atUnsafe(0),
              msg.length.toULong,
              pubKey.atUnsafe(0)
            ) == 0
}

object SHA3 {
    @link("tiny_keccak_wrapper")
    @extern
    object LibTinyKeccak {
        def sha3_256(data: Ptr[Byte], len: CSize, out: Ptr[Byte]): Unit = extern
        def keccak_256(data: Ptr[Byte], len: CSize, out: Ptr[Byte]): Unit = extern
    }

    def sha3_256(input: Array[Byte]): Array[Byte] = {
        val output = new Array[Byte](32) // SHA3-256 produces 32 bytes
        // Compute hash
        LibTinyKeccak.sha3_256(input.atUnsafe(0), input.length.toCSize, output.atUnsafe(0))
        output
    }

    def keccak256(input: Array[Byte]): Array[Byte] = {
        val output = new Array[Byte](32)
        LibTinyKeccak.keccak_256(input.atUnsafe(0), input.length.toCSize, output.atUnsafe(0))
        output
    }
}

trait NativePlatformSpecific extends PlatformSpecific {
    override def sha2_256(bs: ByteString): ByteString =
        ByteString.unsafeFromArray(Sodium.sha256(bs.bytes))

    override def sha3_256(bs: ByteString): ByteString =
        ByteString.unsafeFromArray(SHA3.sha3_256(bs.bytes))

    override def blake2b_224(bs: ByteString): ByteString =
        ByteString.unsafeFromArray(Sodium.blake2b224(bs.bytes))

    override def blake2b_256(bs: ByteString): ByteString =
        ByteString.unsafeFromArray(Sodium.blake2b256(bs.bytes))

    override def verifyEd25519Signature(pk: ByteString, msg: ByteString, sig: ByteString): Boolean =
        require(pk.length == 32, s"Invalid public key length ${pk.length}")
        require(sig.length == 64, s"Invalid signature length ${sig.length}")
        Sodium.verifyEd25519Signature(pk.bytes, msg.bytes, sig.bytes)

    override def verifyEcdsaSecp256k1Signature(
        pk: ByteString,
        msg: ByteString,
        sig: ByteString
    ): Boolean =
        ???

    override def verifySchnorrSecp256k1Signature(
        pk: ByteString,
        msg: ByteString,
        sig: ByteString
    ): Boolean = ???

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

    override def bls12_381_G1_compress(p: BLS12_381_G1_Element): ByteString = ???

    override def bls12_381_G1_uncompress(bs: ByteString): BLS12_381_G1_Element =
        ???
    override def bls12_381_G1_hashToGroup(bs: ByteString, dst: ByteString): BLS12_381_G1_Element =
        ???
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
    override def bls12_381_G2_compress(p: BLS12_381_G2_Element): ByteString = ???
    override def bls12_381_G2_uncompress(bs: ByteString): BLS12_381_G2_Element =
        ???
    override def bls12_381_G2_hashToGroup(bs: ByteString, dst: ByteString): BLS12_381_G2_Element =
        ???
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
    override def bls12_381_finalVerify(p1: BLS12_381_MlResult, p2: BLS12_381_MlResult): Boolean =
        ???
    override def keccak_256(bs: ByteString): ByteString =
        ByteString.unsafeFromArray(SHA3.keccak256(bs.bytes))
}

object NativePlatformSpecific extends NativePlatformSpecific

given PlatformSpecific = NativePlatformSpecific
