package scalus.builtin

object JSPlatformSpecific extends JSPlatformSpecific
trait JSPlatformSpecific extends PlatformSpecific {
    override def sha2_256(bs: ByteString): ByteString = ???

    override def sha3_256(bs: ByteString): ByteString = ???

    override def verifySchnorrSecp256k1Signature(
        pk: ByteString,
        msg: ByteString,
        sig: ByteString
    ): Boolean = ???

    override def blake2b_224(bs: ByteString): ByteString = ???
    override def blake2b_256(bs: ByteString): ByteString = ???

    override def verifyEd25519Signature(pk: ByteString, msg: ByteString, sig: ByteString): Boolean =
        ???

    override def verifyEcdsaSecp256k1Signature(
        pk: ByteString,
        msg: ByteString,
        sig: ByteString
    ): Boolean = ???

}

given PlatformSpecific = JSPlatformSpecific
