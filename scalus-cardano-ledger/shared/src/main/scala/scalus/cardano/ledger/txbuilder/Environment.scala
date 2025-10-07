package scalus.cardano.ledger.txbuilder

import scalus.cardano.address.Network
import scalus.cardano.ledger.{Era, PlutusScriptEvaluator, ProtocolParams}

case class Environment(
    protocolParams: ProtocolParams,
    evaluator: PlutusScriptEvaluator,
    network: Network,
    era: Era = Era.Conway,
)
