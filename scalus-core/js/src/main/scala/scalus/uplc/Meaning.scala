package scalus.uplc

import scalus.builtin.NodeJsPlatformSpecific

object Meaning {
    lazy val allBuiltins: BuiltinsMeaning = BuiltinsMeaning(
      eval.BuiltinCostModel.defaultCostModelC,
      NodeJsPlatformSpecific,
      BuiltinSemanticsVariant.C
    )
}
