package scalus.uplc

import scalus.builtin.{platform, PlatformSpecific}

object Meaning {
    lazy val allBuiltins: CardanoBuiltins = CardanoBuiltins(
      eval.BuiltinCostModel.defaultCostModelC,
      platform,
      BuiltinSemanticsVariant.C
    )
}
