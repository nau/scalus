package scalus.cardano.ledger

import io.bullet.borer.*

import scala.collection.immutable.Set

case class TransactionBody(
    /** Transaction inputs to spend */
    inputs: Set[TransactionInput],

    /** Transaction outputs to create */
    outputs: IndexedSeq[Sized[TransactionOutput]],

    /** Transaction fee */
    fee: Coin,

    /** Time-to-live (TTL) - transaction is invalid after this slot */
    ttl: Option[Long] = None,

    /** Certificates for delegation, stake operations, etc. */
    certificates: Set[Certificate] = Set.empty,

    /** Withdrawals from reward accounts */
    withdrawals: Option[Withdrawals] = None,

    /** Auxiliary data hash */
    auxiliaryDataHash: Option[AuxiliaryDataHash] = None,

    /** Transaction validity start (transaction is invalid before this slot) */
    validityStartSlot: Option[Long] = None,

    /** Minting operations */
    mint: Option[Mint] = None,

    /** Script data hash */
    scriptDataHash: Option[ScriptDataHash] = None,

    /** Collateral inputs */
    collateralInputs: Set[TransactionInput] = Set.empty,

    /** Required signers */
    requiredSigners: Set[AddrKeyHash] = Set.empty,

    /** Network ID */
    networkId: Option[Int] = None,

    /** Collateral return output */
    collateralReturnOutput: Option[Sized[TransactionOutput]] = None,

    /** Total collateral */
    totalCollateral: Option[Coin] = None,

    /** Reference inputs */
    referenceInputs: Set[TransactionInput] = Set.empty,

    /** Voting procedures */
    votingProcedures: Option[VotingProcedures] = None,

    /** Proposal procedures */
    proposalProcedures: Set[ProposalProcedure] = Set.empty,

    /** Transaction deposit */
    currentTreasuryValue: Option[Coin] = None,

    /** Transaction deposit return */
    donation: Option[Coin] = None
):
    /** Validate optional withdrawals */
    require(
      withdrawals.forall(_.withdrawals.nonEmpty),
      "If withdrawals are present, they must be non-empty"
    )

    /** Validate network ID if present */
    require(
      networkId.forall(id => id == 0 || id == 1),
      "Network ID must be 0 (testnet) or 1 (mainnet)"
    )

    /** Validate deposit and deposit return */
    require(
      !(currentTreasuryValue.isDefined && donation.isEmpty) &&
          !(currentTreasuryValue.isEmpty && donation.isDefined),
      "currentTreasuryValue and deposit return must both be defined or both be undefined"
    )

object TransactionBody:
    /** CBOR encoder for TransactionBody */
    given Encoder[TransactionBody] with
        def write(w: Writer, value: TransactionBody): Writer =
            // Count the number of fields to write
            var mapSize = 3 // inputs, outputs, fee are required

            if value.ttl.isDefined then mapSize += 1
            if value.certificates.nonEmpty then mapSize += 1
            if value.withdrawals.isDefined then mapSize += 1
            if value.auxiliaryDataHash.isDefined then mapSize += 1
            if value.validityStartSlot.isDefined then mapSize += 1
            if value.mint.isDefined then mapSize += 1
            if value.scriptDataHash.isDefined then mapSize += 1
            if value.collateralInputs.nonEmpty then mapSize += 1
            if value.requiredSigners.nonEmpty then mapSize += 1
            if value.networkId.isDefined then mapSize += 1
            if value.collateralReturnOutput.isDefined then mapSize += 1
            if value.totalCollateral.isDefined then mapSize += 1
            if value.referenceInputs.nonEmpty then mapSize += 1
            if value.votingProcedures.isDefined then mapSize += 1
            if value.proposalProcedures.nonEmpty then mapSize += 1
            if value.currentTreasuryValue.isDefined then mapSize += 1
            if value.donation.isDefined then mapSize += 1

            w.writeMapHeader(mapSize)

            // Required fields

            // Inputs (key 0)
            w.writeInt(0)
            writeSet(w, value.inputs)

            // Outputs (key 1)
            w.writeInt(1)
            w.write(value.outputs)

            // Fee (key 2)
            w.writeInt(2)
            w.write(value.fee)

            // Optional fields

            // TTL (key 3)
            value.ttl.foreach { ttl =>
                w.writeInt(3)
                w.writeLong(ttl)
            }

            // Certificates (key 4)
            if value.certificates.nonEmpty then
                w.writeInt(4)
                writeSet(w, value.certificates)

            // Withdrawals (key 5)
            value.withdrawals.foreach { withdrawals =>
                w.writeInt(5)
                w.write(withdrawals)
            }

            // Auxiliary data hash (key 7)
            value.auxiliaryDataHash.foreach { hash =>
                w.writeInt(7)
                w.write(hash)
            }

            // Validity start slot (key 8)
            value.validityStartSlot.foreach { slot =>
                w.writeInt(8)
                w.writeLong(slot)
            }

            // Mint (key 9)
            value.mint.foreach { mint =>
                w.writeInt(9)
                w.write(mint)
            }

            // Script data hash (key 11)
            value.scriptDataHash.foreach { hash =>
                w.writeInt(11)
                w.write(hash)
            }

            // Collateral inputs (key 13)
            if value.collateralInputs.nonEmpty then
                w.writeInt(13)
                writeSet(w, value.collateralInputs)

            if value.requiredSigners.nonEmpty then
                // Required signers (key 14)
                w.writeInt(14)
                writeSet(w, value.requiredSigners)

            // Network ID (key 15)
            value.networkId.foreach { id =>
                w.writeInt(15)
                w.writeInt(id)
            }

            // Collateral return output (key 16)
            value.collateralReturnOutput.foreach { output =>
                w.writeInt(16)
                w.write(output)
            }

            // Total collateral (key 17)
            value.totalCollateral.foreach { coin =>
                w.writeInt(17)
                w.write(coin)
            }

            // Reference inputs (key 18)
            if value.referenceInputs.nonEmpty then
                w.writeInt(18)
                writeSet(w, value.referenceInputs)

            // Voting procedures (key 19)
            value.votingProcedures.foreach { procedures =>
                w.writeInt(19)
                w.write(procedures)
            }

            // Proposal procedures (key 20)
            if value.proposalProcedures.nonEmpty then
                w.writeInt(20)
                writeSet(w, value.proposalProcedures)

            // Deposit (key 21)
            value.currentTreasuryValue.foreach { coin =>
                w.writeInt(21)
                w.write(coin)
            }

            // Deposit return (key 22)
            value.donation.foreach { coin =>
                w.writeInt(22)
                w.write(coin)
            }

            w

    /** Helper to write a Set as CBOR */
    private def writeSet[A](w: Writer, set: Set[A])(using encoder: Encoder[A]): Writer =
        w.writeTag(Tag.Other(258))
        w.writeArrayHeader(set.size)
        set.foreach(encoder.write(w, _))
        w

    /** CBOR decoder for TransactionBody */
    given (using OriginalCborByteArray): Decoder[TransactionBody] with
        def read(r: Reader): TransactionBody =
            val mapSize = r.readMapHeader()

            var inputs = Set.empty[TransactionInput]
            var outputs = IndexedSeq.empty[Sized[TransactionOutput]]
            var fee: Option[Coin] = None
            var ttl: Option[Long] = None
            var certificates = Set.empty[Certificate]
            var withdrawals: Option[Withdrawals] = None
            var auxiliaryDataHash: Option[AuxiliaryDataHash] = None
            var validityStartSlot: Option[Long] = None
            var mint: Option[Mint] = None
            var scriptDataHash: Option[ScriptDataHash] = None
            var collateralInputs = Set.empty[TransactionInput]
            var requiredSigners = Set.empty[AddrKeyHash]
            var networkId: Option[Int] = None
            var collateralReturnOutput: Option[Sized[TransactionOutput]] = None
            var totalCollateral: Option[Coin] = None
            var referenceInputs = Set.empty[TransactionInput]
            var votingProcedures: Option[VotingProcedures] = None
            var proposalProcedures = Set.empty[ProposalProcedure]
            var currentTreasuryValue: Option[Coin] = None
            var donation: Option[Coin] = None

            var i = 0L
            while i < mapSize do
                val key = r.readInt()

                key match
                    case 0 => // Inputs
                        inputs = readSet[TransactionInput](r)

                    case 1 => // Outputs
                        outputs = r.read[IndexedSeq[Sized[TransactionOutput]]]()

                    case 2 => // Fee
                        fee = Some(r.read[Coin]())

                    case 3 => // TTL
                        if r.hasOverLong then ttl = Some(r.readOverLong())
                        else ttl = Some(r.readLong())
                    case 4 => // Certificates
                        certificates = readSet[Certificate](r)

                    case 5 => // Withdrawals
                        withdrawals = Some(r.read[Withdrawals]())

                    case 7 => // Auxiliary data hash
                        auxiliaryDataHash = Some(r.read[AuxiliaryDataHash]())

                    case 8 => // Validity start slot
                        validityStartSlot = Some(r.readLong())

                    case 9 => // Mint
                        mint = Some(r.read[MultiAsset]())

                    case 11 => // Script data hash
                        scriptDataHash = Some(r.read[ScriptDataHash]())

                    case 13 => // Collateral inputs
                        collateralInputs = readSet(r)

                    case 14 => // Required signers
                        requiredSigners = readSet(r)

                    case 15 => // Network ID
                        val id = r.readInt()
                        if id != 0 && id != 1 then
                            r.validationFailure(s"Network ID must be 0 or 1, got $id")
                        networkId = Some(id)

                    case 16 => // Collateral return output
                        collateralReturnOutput = Some(r.read[Sized[TransactionOutput]]())

                    case 17 => // Total collateral
                        totalCollateral = Some(r.read[Coin]())

                    case 18 => // Reference inputs
                        referenceInputs = readSet(r)

                    case 19 => // Voting procedures
                        votingProcedures = Some(r.read[VotingProcedures]())

                    case 20 => // Proposal procedures
                        proposalProcedures = readSet[ProposalProcedure](r)
                    case 21 => // Deposit
                        currentTreasuryValue = Some(r.read[Coin]())

                    case 22 => // Deposit return
                        donation = Some(r.read[Coin]())

                    case _ =>
                        r.skipElement()
                i += 1
            end while
            TransactionBody(
              inputs = inputs,
              outputs = outputs,
              fee = fee.get,
              ttl = ttl,
              certificates = certificates,
              withdrawals = withdrawals,
              auxiliaryDataHash = auxiliaryDataHash,
              validityStartSlot = validityStartSlot,
              mint = mint,
              scriptDataHash = scriptDataHash,
              collateralInputs = collateralInputs,
              requiredSigners = requiredSigners,
              networkId = networkId,
              collateralReturnOutput = collateralReturnOutput,
              totalCollateral = totalCollateral,
              referenceInputs = referenceInputs,
              votingProcedures = votingProcedures,
              proposalProcedures = proposalProcedures,
              currentTreasuryValue = currentTreasuryValue,
              donation = donation
            )

    /** Helper to read a Set from CBOR */
    private def readSet[A](r: Reader)(using decoder: Decoder[A]): Set[A] =
        // Check for indefinite array tag (258)
        if r.dataItem() == DataItem.Tag then
            val tag = r.readTag()
            if tag.code != 258 then
                r.validationFailure(s"Expected tag 258 for definite Set, got $tag")
            r.read[Set[A]]()
        else r.read[Set[A]]()
