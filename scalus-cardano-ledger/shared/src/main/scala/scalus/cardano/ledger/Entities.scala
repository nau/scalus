package scalus.cardano.ledger

import scalus.ledger.api.ValidityInterval

sealed abstract class TransactionException(message: String, cause: Throwable)
    extends RuntimeException(message, cause) {
    def this(message: String) = this(message, null)
}

object TransactionException {
    // It's Shelley.InputSetEmptyUTxO in cardano-ledger
    final class EmptyInputsException(val transactionId: TransactionHash)
        extends TransactionException(s"Empty transaction inputs for transactionId $transactionId")

    // It's BabbageNonDisjointRefInputs in cardano-ledger
    final class NonDisjointInputsAndReferenceInputsException(
        val transactionId: TransactionHash,
        val intersection: Set[TransactionInput]
    ) extends TransactionException(
          s"Inputs intersects with reference inputs for transactionId $transactionId, intersection: $intersection"
        )

    // It's Shelley.BadInputsUTxO in cardano-ledger
    final class BadAllInputsUTxOException(
        val transactionId: TransactionHash,
        val missingInputs: Set[TransactionInput],
        val missingCollateralInputs: Set[TransactionInput],
        val missingReferenceInputs: Set[TransactionInput]
    ) extends TransactionException(
          s"Missing inputs, collateral inputs or reference inputs in UTxO state for transactionId $transactionId, missing inputs: $missingInputs, missing collateral inputs: $missingCollateralInputs, missing reference inputs: $missingReferenceInputs"
        )

    final class BadInputsUTxOException(
        val transactionId: TransactionHash
    ) extends TransactionException(
          s"Missing inputs in UTxO state for transactionId $transactionId"
        )

    final class BadCollateralInputsUTxOException(
        val transactionId: TransactionHash
    ) extends TransactionException(
          s"Missing collateral inputs in UTxO state for transactionId $transactionId"
        )

    final class BadReferenceInputsUTxOException(
        val transactionId: TransactionHash
    ) extends TransactionException(
          s"Missing reference inputs in UTxO state for transactionId $transactionId"
        )

    // It's Shelley.InvalidWitnessesUTXOW in cardano-ledger
    final class InvalidSignaturesInWitnessesException(
        val transactionId: TransactionHash,
        val invalidVkeyWitnesses: Set[VKeyWitness],
        val invalidBootstrapWitnesses: Set[BootstrapWitness]
    ) extends TransactionException(
          s"Invalid verified signatures in witnesses for transactionId $transactionId, invalid vkey witnesses: $invalidVkeyWitnesses, invalid bootstrap witnesses: $invalidBootstrapWitnesses"
        )

    // It's Shelley.MissingVKeyWitnessesUTXOW in cardano-ledger
    final class MissingKeyHashesException(
        val transactionId: TransactionHash,
        val missingInputsKeyHashes: Set[AddrKeyHash | StakeKeyHash],
        val missingCollateralInputsKeyHashes: Set[AddrKeyHash | StakeKeyHash],
        val missingVotingProceduresKeyHashes: Set[AddrKeyHash],
        val missingWithdrawalsKeyHashes: Set[AddrKeyHash | StakeKeyHash],
        val missingCertificatesKeyHashes: Set[AddrKeyHash | PoolKeyHash],
        val missingRequiredSignersKeyHashes: Set[AddrKeyHash]
    ) extends TransactionException(
          s"Missing key hashes for transactionId $transactionId, missing inputs key hashes: $missingInputsKeyHashes, missing collateral inputs key hashes: $missingCollateralInputsKeyHashes, missing voting procedures key hashes: $missingVotingProceduresKeyHashes, missing withdrawals key hashes: $missingWithdrawalsKeyHashes, missing certificates key hashes: $missingCertificatesKeyHashes, missing required signers key hashes: $missingRequiredSignersKeyHashes"
        )

    // It's Shelley.MissingScriptWitnessesUTXOW and Shelley.ExtraneousScriptWitnessesUTXOW in cardano-ledger
    final class MissingOrExtraScriptHashesException(
        val transactionId: TransactionHash,
        val missingInputsScriptHashes: Set[ScriptHash],
        val missingMintScriptHashes: Set[ScriptHash],
        val missingVotingProceduresScriptHashes: Set[ScriptHash],
        val missingWithdrawalsScriptHashes: Set[ScriptHash],
        val missingProposalProceduresScriptHashes: Set[ScriptHash],
        val missingCertificatesScriptHashes: Set[ScriptHash],
        val extraScriptHashes: Set[ScriptHash]
    ) extends TransactionException(
          s"Missing or extra script hashes for transactionId $transactionId, missing inputs script hashes: $missingInputsScriptHashes, missing mint script hashes: $missingMintScriptHashes, missing voting procedures script hashes: $missingVotingProceduresScriptHashes, missing withdrawals script hashes: $missingWithdrawalsScriptHashes, missing proposal procedures script hashes: $missingProposalProceduresScriptHashes, missing certificates script hashes: $missingCertificatesScriptHashes, extra script hashes: $extraScriptHashes"
        )

    // It's Shelley.ScriptWitnessNotValidatingUTXOW in cardano-ledger
    final class NativeScriptsException(
        val transactionId: TransactionHash,
        val invalidWitnessesNativeScripts: Set[ScriptHash],
        val invalidProvidedReferenceNativeScripts: Set[ScriptHash],
    ) extends TransactionException(
          s"Invalid native scripts for transactionId $transactionId, invalid witnesses native scripts: $invalidWitnessesNativeScripts, invalid provided reference native scripts: $invalidProvidedReferenceNativeScripts"
        )

    // It's Shelley.MaxTxSizeUTxO in cardano-ledger
    final class InvalidTransactionSizeException(
        val transactionId: TransactionHash,
        val transactionSize: Int,
        val maxTransactionSize: Long
    ) extends TransactionException(
          s"Transaction size $transactionSize exceeds maximum allowed size $maxTransactionSize for transactionId $transactionId"
        )

    // It's BabbageOutputTooSmallUTxO in cardano-ledger
    final class OutputsHaveNotEnoughCoinsException(
        val transactionId: TransactionHash,
        val invalidOutputs: Seq[(TransactionOutput, Coin)],
        val invalidCollateralOutput: Option[(TransactionOutput, Coin)]
    ) extends TransactionException(
          s"Transaction outputs are too small for transactionId $transactionId, invalid outputs: $invalidOutputs, invalid collateral output: $invalidCollateralOutput"
        )

    // It's Alonzo.OutputTooBigUTxO in cardano-ledger
    final class OutputsHaveTooBigValueStorageSizeException(
        val transactionId: TransactionHash,
        val maxValueSize: Long,
        val invalidOutputs: Seq[(TransactionOutput, Int)],
        val invalidCollateralOutput: Option[(TransactionOutput, Int)]
    ) extends TransactionException(
          s"Transaction outputs exceed maximum value storage size $maxValueSize for transactionId $transactionId, invalid outputs: $invalidOutputs, invalid collateral output: $invalidCollateralOutput"
        )

    // It's Allegra.OutsideValidityIntervalUTxO in cardano-ledger
    final class OutsideValidityIntervalException(
        val transactionId: TransactionHash,
        val validityInterval: ValidityInterval,
        val slot: SlotNo
    ) extends TransactionException(
          s"Transaction $transactionId is outside the validity interval $validityInterval for slot $slot"
        )

    // It's Shelley.ValueNotConservedUTxO in cardano-ledger
    final class ValueNotConservedUTxOException(
        val transactionId: TransactionHash,
        val consumed: Value,
        val produced: Value
    ) extends TransactionException(
          s"Value not conserved for transactionId $transactionId, consumed: $consumed, produced: $produced"
        )

    // It's Babbage.FeeTooSmallUTxO in cardano-ledger
    final class FeeTooSmallException(
        val transactionId: TransactionHash,
        val transactionFee: Coin,
        val minTransactionFee: Coin
    ) extends TransactionException(
          s"Transaction fee $transactionFee is too small, minimum required fee is $minTransactionFee for transactionId $transactionId"
        )

    // It's Alonzo.ScriptsNotPaidUTxO in cardano-ledger
    final class CollateralsConsistNotOnlyVKeyAddressException(
        val transactionId: TransactionHash,
        val invalidCollaterals: Set[(TransactionInput, TransactionOutput)]
    ) extends TransactionException(
          s"Collaterals consist not only VKey addresses for transactionId $transactionId, invalid collaterals: $invalidCollaterals"
        )

    // It's Babbage.CollateralContainsNonADA in cardano-ledger
    final class CollateralsContainNotOnlyADAException(
        val transactionId: TransactionHash,
        val invalidCollaterals: Set[(TransactionInput, TransactionOutput)],
        val collateralReturnOutput: Option[TransactionOutput]
    ) extends TransactionException(
          s"Collaterals contain non-ADA assets for transactionId $transactionId, invalid collaterals: $invalidCollaterals, collateral return output: $collateralReturnOutput"
        )

    // It's Alonzo.InsufficientCollateral in cardano-ledger
    final class InsufficientTotalSumOfCollateralCoinsException(
        val transactionId: TransactionHash,
        val totalSumOfCollateralCoins: Coin,
        val collateralReturnOutput: Option[TransactionOutput],
        val transactionFee: Coin,
        val collateralPercentage: Long
    ) extends TransactionException(
          s"Insufficient total sum of collateral coins for transactionId $transactionId, total sum of collateral coins: $totalSumOfCollateralCoins, collateral return output: $collateralReturnOutput, transaction fee: $transactionFee, collateral percentage: $collateralPercentage"
        )

    // It's Babbage.IncorrectTotalCollateralField in cardano-ledger
    final class IncorrectTotalCollateralException(
        val transactionId: TransactionHash,
        val totalSumOfCollateralCoins: Coin,
        val totalCollateral: Option[Coin]
    ) extends TransactionException(
          s"Incorrect total collateral for transactionId $transactionId, total sum of collateral coins: $totalSumOfCollateralCoins, total collateral: $totalCollateral"
        )

    // It's Babbage.NoCollateralInputs in cardano-ledger
    final class NoCollateralInputsException(val transactionId: TransactionHash)
        extends TransactionException(s"No collateral inputs for transactionId $transactionId")

    // TODO placeholder for general exception, remove after finishing development
    final class IllegalArgumentException(message: String) extends TransactionException(message)
}

type UTxO = Map[TransactionInput, TransactionOutput]
type SlotNo = Long
type GovState = Unit
type StakeMap = Map[Credential, Coin]
case class UTxOState(
    utxo: UTxO, // UtxO entries
    deposited: Coin, // Lazy field used only for assertions
    fees: Coin, // Accumulated transaction fees
    govState: GovState, // Governance state
    stakeDistribution: StakeMap, // Stake distribution
    donation: Coin // Donation amount
)

/*
data ConwayCertState era = ConwayCertState
    { conwayCertVState :: !(VState era)
        , conwayCertPState :: !(PState era)
        , conwayCertDState :: !(DState era)
    }
 */

case class CertState(
    vstate: VotingState,
    pstate: PoolsState,
    dstate: DelegationState
)
object CertState {
    def empty: CertState = CertState(
      VotingState(Map.empty),
      PoolsState(),
      DelegationState(Map.empty, Map.empty, Map.empty, Map.empty)
    )
}

/*
 * -- | The state that tracks the voting entities (DReps and Constitutional Committee
-- members). In the formal ledger specification this type is called @GState@
data VState era = VState
  { vsDReps :: !(Map (Credential 'DRepRole) DRepState)
  , vsCommitteeState :: !(CommitteeState era)
  , vsNumDormantEpochs :: !EpochNo
  -- ^ Number of contiguous epochs in which there are exactly zero
  -- active governance proposals to vote on. It is incremented in every
  -- EPOCH rule if the number of active governance proposals to vote on
  -- continues to be zero. It is reset to zero when a new governance
  -- action is successfully proposed. We need this counter in order to
  -- bump DRep expiries through dormant periods when DReps do not have
  -- an opportunity to vote on anything.
  }*/

case class VotingState(
    dreps: Map[Credential, DRepState],
    //    vsCommitteeState: CommitteeState,
    //    vsNumDormantEpochs: EpochNo
)

/*
 * data DRepState = DRepState
  { drepExpiry :: !EpochNo
  , drepAnchor :: !(StrictMaybe Anchor)
  , drepDeposit :: !Coin
  , drepDelegs :: !(Set (Credential 'Staking))
  }
 */
type EpochNo = Long
case class DRepState(
    expiry: EpochNo,
    anchor: Option[Anchor],
    deposit: Coin,
    delegates: Set[Credential]
)

/*-- | The state used by the POOL rule, which tracks stake pool information.
data PState era = PState
  { psStakePoolParams :: !(Map (KeyHash 'StakePool) PoolParams)
  -- ^ The stake pool parameters.
  , psFutureStakePoolParams :: !(Map (KeyHash 'StakePool) PoolParams)
  -- ^ The future stake pool parameters.
  -- Changes to existing stake pool parameters are staged in order
  -- to give delegators time to react to changes.
  -- See section 11.2, "Example Illustration of the Reward Cycle",
  -- of the Shelley Ledger Specification for a sequence diagram.
  , psRetiring :: !(Map (KeyHash 'StakePool) EpochNo)
  -- ^ A map of retiring stake pools to the epoch when they retire.
  , psDeposits :: !(Map (KeyHash 'StakePool) Coin)
  -- ^ A map of the deposits for each pool
  }
 */

case class PoolsState(
//    psStakePoolParams: Map[AddrKeyHash, PoolParams],
//    psFutureStakePoolParams: Map[AddrKeyHash, PoolParams],
//    psRetiring: Map[AddrKeyHash, EpochNo],
//    psDeposits: Map[AddrKeyHash, Coin]
)

/*-- | The state used by the DELEG rule, which roughly tracks stake
-- delegation and some governance features.
data DState era = DState
  { dsUnified :: !UMap
  -- ^ Unified Reward Maps. This contains the reward map (which is the source
  -- of truth regarding the registered stake credentials, the deposit map,
  -- the delegation map, and the stake credential pointer map.
  , dsFutureGenDelegs :: !(Map FutureGenDeleg GenDelegPair)
  -- ^ Future genesis key delegations
  , dsGenDelegs :: !GenDelegs
  -- ^ Genesis key delegations
  , dsIRewards :: !InstantaneousRewards
  -- ^ Instantaneous Rewards
  }
 */

case class FutureGenDeleg(slot: Slot, genesisKeyHash: AddrKeyHash)

/** Delegation State */
case class DelegationState(
    rewards: Map[Credential, Coin], // Rewards map
    deposits: Map[Credential, Coin], // Deposits map
    stakePools: Map[Credential, PoolKeyHash], // Delegation map
    dreps: Map[Credential, DRep],
//    futureGenDelegs: Map[FutureGenDeleg, GenDelegPair],
//    genDelegs: GenDelegs,
//    instantaneousRewards: InstantaneousRewards
)
