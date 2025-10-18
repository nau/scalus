package scalus.uplc

import scalus.builtin.{platform, PlatformSpecific}
import scalus.cardano.ledger.{CardanoInfo, Language}
import scalus.uplc.eval.MachineParams.fromProtocolParams

object Meaning {
    lazy val allBuiltins: CardanoBuiltins = CardanoBuiltins(
      fromProtocolParams(CardanoInfo.mainnet.protocolParams, Language.PlutusV3).builtinCostModel,
      platform,
      BuiltinSemanticsVariant.C
    )
}
