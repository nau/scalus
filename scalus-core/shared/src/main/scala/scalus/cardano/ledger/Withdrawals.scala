package scalus.cardano.ledger

import io.bullet.borer.{Decoder, Encoder, Reader, Writer}

import scala.collection.immutable.SortedMap

/** Represents withdrawals in a transaction.
  *
  * Withdrawals is a map from reward accounts to withdrawal amounts. It's used to withdraw staking
  * rewards.
  *
  * @param withdrawals
  *   Map from reward accounts to withdrawal amounts
  */
case class Withdrawals(withdrawals: SortedMap[RewardAccount, Coin]) {

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
    val empty: Withdrawals = Withdrawals(SortedMap.empty)

    /** CBOR Encoder for Withdrawals. Encodes as a map from reward accounts to coin values.
      */
    given Encoder[Withdrawals] = (w: Writer, value: Withdrawals) => {
        w.write(value.withdrawals)
    }

    /** CBOR Decoder for Withdrawals. Decodes from a map from reward accounts to coin values.
      */
    given Decoder[Withdrawals] = (r: Reader) => {
        Withdrawals(SortedMap.from(r.read[Map[RewardAccount, Coin]]()))
    }
}
