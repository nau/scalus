package scalus.cardano.ledger
package rules

import scalus.ledger.babbage.ProtocolParams

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

trait STS {
    type State
    type Event
    type Error

    def transit(state: State, event: Event): Either[Error, State]
    final def apply(state: State, event: Event): Either[Error, State] = transit(state, event)

//    final def |>>[T <: STS](other: T)(using ev: T =:= this.type): STS = (state, event) =>
//        for
//            state <- this.transit(state, event)
//            state <- other.transit(state, event)
//        yield state

}

trait Validator extends STS {

    override final def transit(state: State, event: Event): Either[Error, State] = {
        validate(state, event) match {
            case Right(_)    => Right(state)
            case Left(error) => Left(error)
        }
    }

    def validate(state: State, event: Event): Either[Error, Unit]
}

trait LedgerSTS extends STS {
    override final type State = LedgerState
    override final type Event = Transaction
    override final type Error = Throwable
}

trait LedgerValidator extends Validator {
    override final type State = LedgerState
    override final type Event = Transaction
    override final type Error = Throwable
}
