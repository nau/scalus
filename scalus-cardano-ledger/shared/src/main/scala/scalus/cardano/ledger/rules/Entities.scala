package scalus.cardano.ledger
package rules

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
    type Value
    type Error = TransactionException | Throwable
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
