package scalus.examples

import scalus.*
import scalus.builtin.Data
import scalus.ledger.api.v3.{PubKeyHash, TxInfo, TxOutRef}
import scalus.prelude.*

/** A simple validator that checks if the redeemer is "Hello, World!" and if the transaction is
  * signed by the owner.
  */
@Compile
object HelloCardano extends Validator {
    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val owner = datum.getOrFail("Datum not found").to[PubKeyHash]
        val signed = tx.signatories.contains(owner)
        require(signed, "Must be signed")
        val saysHello = redeemer.to[String] == "Hello, World!"
        require(saysHello, "Invalid redeemer")
    }
}
