package scalus.uplc

import scalus.builtin.JVMPlatformSpecific

object Meaning {
    lazy val allBuiltins: BuiltinsMeaning = CardanoBuiltins(
      eval.BuiltinCostModel.defaultCostModelC,
      JVMPlatformSpecific,
      BuiltinSemanticsVariant.C
    )
}
