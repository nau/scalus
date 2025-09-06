package scalus.uplc

import scalus.builtin.JVMPlatformSpecific
import scalus.ledger.api.BuiltinSemanticsVariant

object Meaning {
    lazy val allBuiltins: BuiltinsMeaning = BuiltinsMeaning(
      eval.BuiltinCostModel.defaultCostModelC,
      JVMPlatformSpecific,
      BuiltinSemanticsVariant.C
    )
}
