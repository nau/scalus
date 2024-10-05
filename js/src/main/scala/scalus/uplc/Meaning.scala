package scalus.uplc

import scalus.builtin.NodeJsPlatformSpecific
import scalus.ledger.api.BuiltinSemanticsVariant

object Meaning {
    @deprecated("Will be removed", "0.8.0")
    lazy val defaultBuiltins: BuiltinsMeaning = allBuiltins

    lazy val allBuiltins: BuiltinsMeaning = BuiltinsMeaning(
      eval.BuiltinCostModel.defaultCostModelC,
      NodeJsPlatformSpecific,
      BuiltinSemanticsVariant.C
    )
}
