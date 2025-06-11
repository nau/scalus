package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*

/** Represents a complete block in the Cardano blockchain
  *
  * Valid blocks must also satisfy the following constraints:
  *   1. The length of transaction_bodies and transaction_witness_sets must be the same 2. Every
  *      transaction_index must be strictly smaller than the length of transaction_bodies
  */
case class Block(
    /** Block header */
    header: BlockHeader,

    /** Transaction bodies in this block */
    transactionBodies: IndexedSeq[TransactionBody],

    /** Transaction witness sets for each transaction */
    transactionWitnessSets: IndexedSeq[TransactionWitnessSet],

    /** Auxiliary data associated with transactions by index */
    auxiliaryDataSet: Map[Int, AuxiliaryData],

    /** List of invalid transaction indices */
    invalidTransactions: IndexedSeq[Int]
) derives Codec:
    require(
      transactionBodies.size == transactionWitnessSets.size,
      s"Number of transaction bodies (${transactionBodies.size}) must match number of witness sets (${transactionWitnessSets.size})"
    )

    require(
      auxiliaryDataSet.keys.forall(idx => idx >= 0 && idx < transactionBodies.size),
      "Transaction index in auxiliary data set must be valid"
    )

    require(
      invalidTransactions.forall(idx => idx >= 0 && idx < transactionBodies.size),
      "Invalid transaction index must be valid"
    )

    /** Get the block number */
    def blockNumber: Long = header.blockNumber

    /** Get the slot number */
    def slot: Long = header.slot

    /** Get the block hash */
    def hash: BlockHash = header.headerBody.blockBodyHash

    /** Get the number of transactions in the block */
    def txCount: Int = transactionBodies.size

    /** Get the number of invalid transactions */
    def invalidTxCount: Int = invalidTransactions.size

    /** Get the number of valid transactions */
    def validTxCount: Int = txCount - invalidTxCount

    /** Check if the block is empty (has no transactions) */
    def isEmpty: Boolean = transactionBodies.isEmpty

    /** Reconstruct complete transactions from bodies, witness sets, and auxiliary data */
    def transactions(using OriginalCborByteArray): Seq[Transaction] =
        transactionBodies.zipWithIndex.map { case (body, idx) =>
            val witnessSet = transactionWitnessSets(idx)
            val auxData = auxiliaryDataSet.get(idx)
            val isValid = !invalidTransactions.contains(idx)

            Transaction(KeepRaw(body), witnessSet, isValid, auxData)
        }

case class BlockFile(era: Int, block: Block) derives Codec
