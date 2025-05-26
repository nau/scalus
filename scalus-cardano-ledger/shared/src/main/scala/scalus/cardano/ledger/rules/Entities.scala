package scalus.cardano.ledger
package rules

import scalus.ledger.babbage.ProtocolParams
export scalus.cardano.ledger.{Transaction, TransactionInput, TransactionOutput}

type V = Option[Throwable]

trait TransitionRule

type Utxo = Map[TransactionInput, TransactionOutput]
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

case class LedgerState(utxo: Utxo = Map.empty)

trait STM {
    type State
    type Event
    type Error

    def transit(state: State, event: Event): Either[Error, State]
    final def apply(state: State, event: Event): Either[Error, State] = transit(state, event)
}

trait LedgerSTM extends STM {
    override final type State = LedgerState
    override final type Event = Transaction
    override final type Error = Throwable
}
