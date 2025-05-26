package scalus.uplc

import scalus.builtin.NativePlatformSpecific
import scalus.ledger.api.BuiltinSemanticsVariant

object Meaning {
    lazy val allBuiltins: BuiltinsMeaning = BuiltinsMeaning(
      eval.BuiltinCostModel.defaultCostModelC,
      NativePlatformSpecific,
      BuiltinSemanticsVariant.C
    )
}
