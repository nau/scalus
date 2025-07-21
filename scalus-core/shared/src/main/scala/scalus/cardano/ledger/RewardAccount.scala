package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import scalus.cardano.address.{Address, StakeAddress}

import scala.math.Ordered.orderingToOrdered

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
object RewardAccount {
    given Ordering[RewardAccount] with
        def compare(x: RewardAccount, y: RewardAccount): Int =
            (x.address, y.address) match
                case (StakeAddress(n1, p1), StakeAddress(n2, p2)) =>
                    n1.compare(n2) match
                        case 0 => p1.asHash.compare(p2.asHash)
                        case c => c
                case _ => // FIXME: consider using ByteString instead of Address
                    throw new IllegalArgumentException(
                      s"Cannot compare RewardAccounts with different address types: $x vs $y"
                    )

}
