package scalus.cardano.ledger
package rules

import scalus.ledger.babbage.ProtocolParams
import upickle.default.read

type V = Option[Throwable]

trait TransitionRule

type UTxO = Map[TransactionInput, TransactionOutput]
type SlotNo = Long
type CertState = Unit
type GovState = Unit
type StakeMap = Map[Credential, Coin]

case class UTxOState(
    utxo: UTxO, // UTxO entries
    deposited: Coin, // Lazy field used only for assertions
    fees: Coin, // Accumulated transaction fees
    govState: GovState, // Governance state
    stakeDistribution: StakeMap, // Stake distribution
    donation: Coin // Donation amount
)

case class UtxoEnv(slot: SlotNo, pparams: ProtocolParams)

trait Rule {
    type State
    type Environment
    type Event
    type Error
    def validate(env: Environment, state: State, event: Event): Either[Error, State]
}

object ConwayTxValidation extends Rule {
    type State = UTxO
    type Environment = UtxoEnv
    type Event = Transaction
    type Error = Throwable

    def validate(env: Environment, state: State, tx: Event): Either[Error, State] = {
        FeesValidation.validate(env, state, tx)
    }
}

object FeesValidation extends Rule {
    type State = UTxO
    type Environment = UtxoEnv
    type Event = Transaction
    type Error = Throwable

    def validate(env: Environment, state: State, tx: Event): Either[Error, State] = {
        if tx.isValid && tx.body.fee.value > 0 then Right(state)
        else Left(new IllegalArgumentException("Invalid transaction"))
    }
}
