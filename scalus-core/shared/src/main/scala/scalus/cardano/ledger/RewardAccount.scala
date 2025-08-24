package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import scalus.cardano.address.{Address, StakeAddress}
import scala.util.control.NonFatal

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
case class RewardAccount(address: StakeAddress) {
    def keyHashOption: Option[AddrKeyHash | StakeKeyHash] = address.keyHashOption
    def scriptHashOption: Option[ScriptHash] = address.scriptHashOption
}

object RewardAccount {
    given Ordering[RewardAccount] with
        def compare(x: RewardAccount, y: RewardAccount): Int =
            (x.address, y.address) match
                case (StakeAddress(n1, p1), StakeAddress(n2, p2)) =>
                    n1.compare(n2) match
                        case 0 => p1.asHash.compare(p2.asHash)
                        case c => c

    given Encoder[RewardAccount] with
        def write(w: Writer, value: RewardAccount): Writer = {
            w.write(value.address: Address)
            w
        }

    /** CBOR decoder for Address */
    given Decoder[RewardAccount] with
        def read(r: Reader): RewardAccount = {
            try RewardAccount(r.read[Address]().asInstanceOf[StakeAddress])
            catch case NonFatal(exception) => r.validationFailure(exception.getMessage)
        }
}
