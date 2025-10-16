package scalus.cardano.ledger.txbuilder

import scalus.cardano.address.Network
import scalus.cardano.ledger.{Era, PlutusScriptEvaluator, ProtocolParams, SlotConfig}

case class Environment(
    protocolParams: ProtocolParams,
    slotConfig: SlotConfig,
    evaluator: PlutusScriptEvaluator,
    network: Network,
    era: Era = Era.Conway,
)
