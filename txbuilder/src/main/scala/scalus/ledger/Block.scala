package scalus.ledger

import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.*

/** Represents a complete block in the Cardano blockchain
  *
  * Valid blocks must also satisfy the following constraints:
  *   1. The length of transaction_bodies and transaction_witness_sets must be the same 2. Every
  *      transaction_index must be strictly smaller than the length of transaction_bodies
  */
case class Block(
    /** Block header */
    header: Header,

    /** Transaction bodies in this block */
    transactionBodies: Seq[TransactionBody],

    /** Transaction witness sets for each transaction */
    transactionWitnessSets: Seq[TransactionWitnessSet],

    /** Auxiliary data associated with transactions by index */
    auxiliaryDataSet: Map[Int, AuxiliaryData],

    /** List of invalid transaction indices */
    invalidTransactions: Seq[Int]
):
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
    def hash: Hash32 = ??? // In a real implementation, this would hash the header

    /** Get the number of transactions in the block */
    def txCount: Int = transactionBodies.size

    /** Get the number of invalid transactions */
    def invalidTxCount: Int = invalidTransactions.size

    /** Get the number of valid transactions */
    def validTxCount: Int = txCount - invalidTxCount

    /** Check if the block is empty (has no transactions) */
    def isEmpty: Boolean = transactionBodies.isEmpty

    /** Reconstruct complete transactions from bodies, witness sets, and auxiliary data */
    def transactions: Seq[Transaction] =
        transactionBodies.zipWithIndex.map { case (body, idx) =>
            val witnessSet = transactionWitnessSets(idx)
            val auxData = auxiliaryDataSet.get(idx)
            val isValid = !invalidTransactions.contains(idx)

            Transaction(body, witnessSet, isValid, auxData)
        }

object Block:
    /** CBOR encoder for Block */
    given Encoder[Block] with
        def write(w: Writer, value: Block): Writer =
            w.writeArrayHeader(5)

            // Header
            Header.given_Encoder_Header.write(w, value.header)

            // Transaction bodies
            w.writeArrayHeader(value.transactionBodies.size)
            value.transactionBodies.foreach(
              TransactionBody.given_Encoder_TransactionBody.write(w, _)
            )

            // Transaction witness sets
            w.writeArrayHeader(value.transactionWitnessSets.size)
            value.transactionWitnessSets.foreach(
              TransactionWitnessSet.given_Encoder_TransactionWitnessSet.write(w, _)
            )

            // Auxiliary data set
            w.writeMapHeader(value.auxiliaryDataSet.size)
            value.auxiliaryDataSet.foreach { case (idx, auxData) =>
                w.writeInt(idx)
                AuxiliaryData.given_Encoder_AuxiliaryData.write(w, auxData)
            }

            // Invalid transactions
            w.writeArrayHeader(value.invalidTransactions.size)
            value.invalidTransactions.foreach(w.writeInt)

            w

    /** CBOR decoder for Block */
    given Decoder[Block] with
        def read(r: Reader): Block =
            val size = r.readArrayHeader()
//            if size != 5 then r.validationFailure(s"Expected 5 elements for Block, got $size")

            // Header
            val header = Header.given_Decoder_Header.read(r)

            // Transaction bodies
            val txBodies = r.read[List[TransactionBody]]()
            val txBodiesSize = txBodies.size

            // Transaction witness sets
            val txWitnesses = r.read[List[TransactionWitnessSet]]()
            val txWitnessSize = txWitnesses.size
            if txWitnessSize != txBodiesSize then
                r.validationFailure(
                  s"Number of transaction bodies ($txBodiesSize) must match number of witness sets ($txWitnessSize)"
                )

            // Auxiliary data set
            val auxData = r.read[Map[Int, AuxiliaryData]]()

            // Invalid transactions
            val invalidTxSize = r.readArrayHeader()
            val invalidTxs = List.newBuilder[Int]
            for _ <- 0L until invalidTxSize do
                val idx = r.readInt()

                if idx < 0 || idx >= txBodiesSize then
                    r.validationFailure(s"Invalid transaction index $idx is out of range")

                invalidTxs += idx

            Block(
              header,
              txBodies,
              txWitnesses,
              auxData,
              invalidTxs.result()
            )

case class BlockFile(era: Int, block: Block) derives Codec