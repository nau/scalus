package scalus.cardano.ledger
package rules

import scalus.ledger.babbage.ProtocolParams
import upickle.default.read

type V = Option[Throwable]

trait TransitionRule

type Utxo = Map[TransactionInput, TransactionOutput]

// It's mutable state for transient calculation
class Context(var fee: Coin = Coin.zero, val env: UtxoEnv = UtxoEnv.default)

case class State(
    utxo: Utxo = Map.empty,
    certState: CertState = CertState.empty,
)

type SlotNo = Long
type GovState = Unit
type StakeMap = Map[Credential, Coin]

case class UTxOState(
    utxo: Utxo, // UtxO entries
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

case class UtxoEnv(slot: SlotNo, params: ProtocolParams, certState: CertState)
object UtxoEnv {
    // TODO: remove
    val default: UtxoEnv =

        val params: ProtocolParams = read[ProtocolParams](
          this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
        )(using ProtocolParams.blockfrostParamsRW)

        // Load protocol parameters from a JSON file
        UtxoEnv(
          0,
          params,
          CertState.empty
        )
}

sealed trait STS {
    final type Context = scalus.cardano.ledger.rules.Context
    final type State = scalus.cardano.ledger.rules.State
    final type Event = Transaction
    type Value <: Unit | State
    final type Error = Throwable
    final type Result = Either[Error, Value]

    def apply(context: Context, state: State, event: Event): Result
}

object STS {
    trait Validator extends STS {
        override final type Value = Unit

        def validate(context: Context, state: State, event: Event): Result

        override final def apply(context: Context, state: State, event: Event): Result =
            validate(context, state, event)

        protected final def failure(error: Error): Result = Left(error)
        protected final val success: Result = Right(())
    }

    trait Mutator extends STS {
        override final type Value = State

        def transit(context: Context, state: State, event: Event): Result

        override final def apply(context: Context, state: State, event: Event): Result =
            transit(context, state, event)

        protected final def failure(error: Error): Result = Left(error)
        protected final def success(state: State): Result = Right(state)
    }
}
