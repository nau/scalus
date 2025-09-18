package scalus.cardano.ledger
package rules

import scalus.cardano.address.Network
import scalus.ledger.babbage.ProtocolParams
import upickle.default.read

// It's mutable state for transient calculation
class Context(
    var fee: Coin = Coin.zero,
    val env: UtxoEnv = UtxoEnv.default,
    val slotConfig: SlotConfig = SlotConfig.Mainnet
)

case class State(
    utxo: UTxO = Map.empty,
    certState: CertState = CertState.empty,
    deposited: Coin = Coin.zero, // Lazy field used only for assertions
    fees: Coin = Coin.zero, // Accumulated transaction fees
    govState: GovState = (), // Governance state
    stakeDistribution: StakeMap = Map.empty, // Stake distribution
    donation: Coin = Coin.zero // Donation amount
)

case class UtxoEnv(slot: SlotNo, params: ProtocolParams, certState: CertState, network: Network)
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
          CertState.empty,
          Network.Testnet
        )
}

sealed trait STS {
    final type Context = scalus.cardano.ledger.rules.Context
    final type State = scalus.cardano.ledger.rules.State
    final type Event = Transaction
    type Value
    type Error <: TransactionException
    final type Result = Either[Error, Value]

    def apply(context: Context, state: State, event: Event): Result

    protected final def failure(error: Error): Result = Left(error)
}

object STS {
    trait Validator extends STS {
        override final type Value = Unit

        def validate(context: Context, state: State, event: Event): Result

        override final def apply(context: Context, state: State, event: Event): Result =
            validate(context, state, event)

        protected final val success: Result = Right(())
    }

    trait Mutator extends STS {
        override final type Value = State

        def transit(context: Context, state: State, event: Event): Result

        override final def apply(context: Context, state: State, event: Event): Result =
            transit(context, state, event)

        protected final def success(state: State): Result = Right(state)
    }
}
