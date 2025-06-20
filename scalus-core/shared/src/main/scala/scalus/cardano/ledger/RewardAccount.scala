package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import scalus.cardano.address.Address

/** Represents a reward account in the Cardano blockchain.
  *
  * Reward accounts (also known as stake addresses) are used to receive staking rewards. They have a
  * specific format with bits 7-5 set to 111 and bit 4 indicating whether the credential is a key
  * hash or script hash.
  *
  * @param address
  *   The address of the reward account
  */
case class RewardAccount(address: Address) derives Codec
