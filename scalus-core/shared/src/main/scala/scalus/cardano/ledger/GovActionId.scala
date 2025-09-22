package scalus.cardano.ledger

import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.*

import scala.math.Ordered.orderingToOrdered

/** Represents a governance action identifier in the Cardano blockchain.
  *
  * A governance action ID consists of a transaction ID and an index, which uniquely identifies a
  * governance action.
  *
  * @param transactionId
  *   The hash of the transaction containing the action
  * @param govActionIndex
  *   The index of the action within the transaction (2 bytes)
  */
case class GovActionId(
    transactionId: TransactionHash,
    govActionIndex: Int
) derives Codec {
    // Validate gov action index range (2 bytes - 0 to 65535)
    require(
      govActionIndex >= 0 && govActionIndex <= 65535,
      s"Gov action index must be between 0 and 65535, got $govActionIndex"
    )
}

object GovActionId {
    given Ordering[GovActionId] with
        def compare(x: GovActionId, y: GovActionId): Int =
            x.transactionId.compareTo(y.transactionId) match
                case 0 => x.govActionIndex.compareTo(y.govActionIndex)
                case c => c
}
