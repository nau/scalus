package scalus.cardano.ledger

import io.bullet.borer.{Decoder, Encoder, Reader, Writer}

import scala.collection.immutable.Map

/** Represents withdrawals in a transaction.
  *
  * Withdrawals is a map from reward accounts to withdrawal amounts. It's used to withdraw staking
  * rewards.
  *
  * @param withdrawals
  *   Map from reward accounts to withdrawal amounts
  */
case class Withdrawals(withdrawals: Map[RewardAccount, Coin]) {

    /** Checks if there are any withdrawals.
      *
      * @return
      *   true if there are no withdrawals, false otherwise
      */
    def isEmpty: Boolean = withdrawals.isEmpty
}

object Withdrawals {

    /** Creates an empty Withdrawals.
      *
      * @return
      *   An empty Withdrawals
      */
    val empty: Withdrawals = Withdrawals(Map.empty)

    /** CBOR Encoder for Withdrawals. Encodes as a map from reward accounts to coin values.
      */
    given Encoder[Withdrawals] = new Encoder[Withdrawals] {
        def write(w: Writer, value: Withdrawals): Writer = {
            w.write(value.withdrawals)
        }
    }

    /** CBOR Decoder for Withdrawals. Decodes from a map from reward accounts to coin values.
      */
    given Decoder[Withdrawals] = new Decoder[Withdrawals] {
        def read(r: Reader): Withdrawals = {
            Withdrawals(r.read[Map[RewardAccount, Coin]]())
        }
    }
}
