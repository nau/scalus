package scalus.builtin

trait NativePlatformSpecific extends PlatformSpecific {
    override def sha2_256(bs: ByteString): ByteString =
        ???

    override def sha3_256(bs: ByteString): ByteString =
        ???

    override def blake2b_224(bs: ByteString): ByteString =
        ???

    override def blake2b_256(bs: ByteString): ByteString =
        ???

    override def verifyEd25519Signature(pk: ByteString, msg: ByteString, sig: ByteString): Boolean =
        if pk.length != 32 then
            throw new IllegalArgumentException(s"Invalid public key length ${pk.length}")
        if sig.length != 64 then
            throw new IllegalArgumentException(s"Invalid signature length ${sig.length}")
        ???

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
        ???
}

object NativePlatformSpecific extends NativePlatformSpecific

given PlatformSpecific = NativePlatformSpecific
