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

trait STS {
    type State
    type Signal
    type Environment
    type Event
    def validate(env: Environment, state: State, signal: Signal): Unit
}

object ConwayTxValidation extends STS {
    type State = UTxO
    type Signal = Transaction
    type Environment = UtxoEnv
    type Event = this.type

    def validate(env: Environment, state: State, tx: Signal): Unit = {
        val inputs = tx.body.inputs ++ tx.body.referenceInputs.getOrElse(Set.empty)
    }
}

object Inputs {
    def validate(env: UtxoEnv, state: UTxO, inputs: Seq[TransactionInput]): Unit = {
        inputs.foreach { input =>
            if !state.contains(input) then {
                throw new IllegalArgumentException(s"Input $input not found in UTxO")
            }
        }
    }
}
