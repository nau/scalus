package scalus.uplc

import scalus.builtin.NodeJsPlatformSpecific
import scalus.ledger.api.BuiltinSemanticsVariant

object Meaning {
    @deprecated("Will be removed", "0.8.0")
    val defaultBuiltins =
        BuiltinsMeaning(
          eval.BuiltinCostModel.defaultCostModel,
          JVMPlatformSpecific,
          BuiltinSemanticsVariant.C
        )
}
