package scalus.ledger

import io.bullet.borer.{Decoder, Encoder, Reader, Writer}
import scalus.builtin.ByteString

/** Represents a reward account in the Cardano blockchain.
  *
  * Reward accounts (also known as stake addresses) are used to receive staking rewards. They have a
  * specific format with bits 7-5 set to 111 and bit 4 indicating whether the credential is a key
  * hash or script hash.
  *
  * @param bytes
  *   The raw bytes of the reward account
  */
case class RewardAccount(bytes: ByteString)

object RewardAccount {

    /** Creates a RewardAccount from a hex string representation.
      *
      * @param hex
      *   A hex string representing the reward account bytes
      * @return
      *   The corresponding RewardAccount
      */
    def fromHex(hex: String): RewardAccount =
        RewardAccount(ByteString.fromHex(hex))

    /** CBOR Encoder for RewardAccount. Encodes as a bytestring containing the reward account bytes.
      */
    given Encoder[RewardAccount] = new Encoder[RewardAccount] {
        def write(w: Writer, value: RewardAccount): Writer =
            w.writeBytes(value.bytes.bytes)
    }

    /** CBOR Decoder for RewardAccount. Decodes from a bytestring containing the reward account
      * bytes.
      */
    given Decoder[RewardAccount] = new Decoder[RewardAccount] {
        def read(r: Reader): RewardAccount =
            RewardAccount(ByteString.unsafeFromArray(r.readBytes()))
    }
}
