package scalus.examples

import scalus.Compile
import scalus.builtin.Data
import scalus.ledger.api.v1.PubKeyHash
import scalus.ledger.api.v3.{TxInfo, TxOutRef}
import scalus.prelude.*
import scalus.prelude.Option.Some

/** https://github.com/blockchain-unica/rosetta-smart-contracts/tree/main/contracts/simple_transfer
  *
  * https://github.com/cardano-foundation/cardano-template-and-ecosystem-monitoring/tree/main/simple-transfer
  *
  * use aiken/crypto.{VerificationKeyHash} use cardano/transaction.{Transaction, OutputReference}
  * use vodka_extra_signatories.{key_signed}
  *
  * // Parameterized validator for simple transfer // This validator checks if the transaction is
  * signed by the receiver's verification key hash // Others can lock funds in the contract, but
  * only the receiver can spend them. // The receiver is specified as a parameter to the validator.
 *
  * validator simpleTransfer(receiver: VerificationKeyHash) {
  *
  * spend(_datum_opt: Option<Data>, _redeemer: Data, _utxo: OutputReference, self: Transaction) {
  * key_signed(self.extra_signatories, receiver) }
  *
 * /// Check if a key is signed by any of the extra_signatories
 * /// ```aiken
 * /// let extra_signatories = ["key1", "key2", "key3"]
 * ///
 * /// let key_to_test_1 = "key2"
 * /// let this_is_true = key_signed(extra_signatories, key_to_test_1)
 * ///
 * /// let key_to_test_2 = "key4"
 * /// let this_is_false = key_signed(extra_signatories, key_to_test_2)
 * /// ```
 * pub fn key_signed(extra_signatories: List<ByteArray>, key: ByteArray) {
 * list.has(extra_signatories, key)
 * }
 *
  * }
  */
@Compile
object SimpleTransfer extends Validator {
    override  def spend( datum: Option[Data],
                           redeemer: Data,
                           tx: TxInfo,
                           ownRef: TxOutRef
                       ): Unit = {
        val Some(ownerData) = datum: @unchecked
        val owner = ownerData.to[PubKeyHash]
        val receiver = redeemer.to[PubKeyHash]
        require(tx.signatories.contains(owner), "Not signed by owner!")
        require(tx.signatories.contains(receiver), "Not signed by receiver!")
    }
}
