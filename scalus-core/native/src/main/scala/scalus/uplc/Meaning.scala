package scalus.uplc

import scalus.builtin.NativePlatformSpecific

object Meaning {
    lazy val allBuiltins: BuiltinsMeaning = CardanoBuiltins(
      eval.BuiltinCostModel.defaultCostModelC,
      NativePlatformSpecific,
      BuiltinSemanticsVariant.C
    )
}
