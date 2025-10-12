package scalus.uplc

import scalus.builtin.NodeJsPlatformSpecific

object Meaning {
    lazy val allBuiltins: BuiltinsMeaning = CardanoBuiltins(
      eval.BuiltinCostModel.defaultCostModelC,
      NodeJsPlatformSpecific,
      BuiltinSemanticsVariant.C
    )
}
