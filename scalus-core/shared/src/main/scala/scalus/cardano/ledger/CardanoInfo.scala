package scalus.cardano.ledger

import scalus.cardano.address.Network

import scala.annotation.threadUnsafe

case class CardanoInfo(protocolParams: ProtocolParams, network: Network) {
    def majorProtocolVersion: MajorProtocolVersion = protocolParams.protocolVersion.toMajor
}

object CardanoInfo {
    @threadUnsafe lazy val mainnet: CardanoInfo =
        CardanoInfo(
          ProtocolParams.fromBlockfrostJson(
            inlineResource("blockfrost-params-epoch-544.json")
          ),
          Network.Mainnet
        )

    private inline def inlineResource(name: String): String =
        ${ scalus.macros.Macros.inlineResource('name) }
}
