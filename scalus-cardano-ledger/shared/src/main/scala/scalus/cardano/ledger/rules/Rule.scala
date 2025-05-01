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

object StateTransition {
    def run(): Unit = {
        val utxo: UTxO = Map.empty
        val input = this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
        val pparams = read[ProtocolParams](input)(using ProtocolParams.blockfrostParamsRW)
        val env = UtxoEnv(slot = 100, pparams = pparams)
        ConwayTxValidation.validate(env, utxo, null)
    }
}
