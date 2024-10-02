package scalus.uplc

import scalus.builtin.JVMPlatformSpecific
import scalus.ledger.api.BuiltinSemanticsVariant

object Meaning {
    @deprecated("Will be removed", "0.8.0")
    val defaultBuiltins =
        BuiltinsMeaning(
          eval.BuiltinCostModel.defaultCostModelC,
          JVMPlatformSpecific,
          BuiltinSemanticsVariant.C
        )
}
