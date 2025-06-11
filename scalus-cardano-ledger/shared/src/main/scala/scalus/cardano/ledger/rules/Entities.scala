package scalus.cardano.ledger
package rules

import scalus.ledger.babbage.ProtocolParams

type V = Option[Throwable]

trait TransitionRule

type Utxo = Map[TransactionInput, TransactionOutput]

// It's mutable state for transient calculation
class Context(var fee: Coin = Coin.zero)

case class State(utxo: Utxo = Map.empty)

type SlotNo = Long
type CertState = Unit
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

case class UtxoEnv(slot: SlotNo, params: ProtocolParams)

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
