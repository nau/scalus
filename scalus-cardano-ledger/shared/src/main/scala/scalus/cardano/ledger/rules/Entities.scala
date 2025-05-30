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

sealed trait STS[StateF[_], EventT, ErrorT] {
    final type StateI[StateT] = StateF[StateT]
    final type Event = EventT
    final type Error = ErrorT
}

object STS {
    trait Validator[StateF[_], EventT, ErrorT] extends STS[StateF, EventT, ErrorT] {
        def validate[StateT: StateI](state: StateT, event: Event): Either[Error, Unit]
    }

    trait Mutator[StateF[_], EventT, ErrorT] extends STS[StateF, EventT, ErrorT] {
        def transit[StateT: StateI](state: StateT, event: Event): Either[Error, StateT]
    }
}

object Ledger {
    type Utxo = Map[TransactionInput, TransactionOutput]

    // TODO think about era or context
    sealed trait StateI[StateT] {
        final type State = StateT
    }

    object StateI {
        type All[StateT] = Utxo[StateT] & Fee[StateT]

        trait Utxo[StateT] extends StateI[StateT] {
            extension (self: State) {
                def utxo: Ledger.Utxo
                def utxo_=(value: Ledger.Utxo): State
            }
        }

        trait Fee[StateT] extends StateI[StateT] {
            extension (self: State) {
                def fee: Coin
                def fee_=(value: Coin): State
            }
        }
    }

    object State {
        final class Default(
            private val _utxo: Ledger.Utxo = Map.empty,
            private val _fee: Coin = Coin.zero
        ) {
            override def toString: String = s"DefaultState{utxo: $_utxo, fee: $_fee}"
        }

        object Default {
            given Ledger.StateI.All[Default] = StateI

            private object StateI extends Ledger.StateI.Utxo[Default], Ledger.StateI.Fee[Default] {
                extension (self: State) {
                    override def utxo: Ledger.Utxo = self._utxo
                    override def utxo_=(value: Ledger.Utxo): State = Default(value, self._fee)
                    override def fee: Coin = self._fee
                    override def fee_=(value: Coin): State = Default(self._utxo, value)
                }
            }
        }
    }

    type Event = Transaction

    // TODO make hierarchy of domain specific errors where high-level ones wrap low-level one
    //  (example transaction error wraps input error)
    type Error = Throwable

    sealed trait STS[StateF[_] <: Ledger.StateI[_]] {
        this: scalus.cardano.ledger.rules.STS[StateF, Ledger.Event, Ledger.Error] =>
    }

    object STS {
        trait Validator[StateF[_] <: Ledger.StateI[_]]
            extends Ledger.STS[StateF],
              scalus.cardano.ledger.rules.STS.Validator[StateF, Ledger.Event, Ledger.Error]

        trait Mutator[StateF[_] <: Ledger.StateI[_]]
            extends Ledger.STS[StateF],
              scalus.cardano.ledger.rules.STS.Mutator[StateF, Ledger.Event, Ledger.Error]
    }
}
