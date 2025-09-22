package scalus.uplc

import scalus.builtin.JVMPlatformSpecific

object Meaning {
    lazy val allBuiltins: BuiltinsMeaning = BuiltinsMeaning(
      eval.BuiltinCostModel.defaultCostModelC,
      JVMPlatformSpecific,
      BuiltinSemanticsVariant.C
    )
}
