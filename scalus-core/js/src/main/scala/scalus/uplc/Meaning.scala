package scalus.uplc

import scalus.builtin.NodeJsPlatformSpecific
import scalus.ledger.api.BuiltinSemanticsVariant

object Meaning {
    lazy val allBuiltins: BuiltinsMeaning = BuiltinsMeaning(
      eval.BuiltinCostModel.defaultCostModelC,
      NodeJsPlatformSpecific,
      BuiltinSemanticsVariant.C
    )
}
