package scalus.uplc

import scalus.builtin.JVMPlatformSpecific
import scalus.ledger.api.BuiltinSemanticsVariant

object Meaning {
    val defaultBuiltins =
        BuiltinsMeaning(
          eval.BuiltinCostModel.defaultCostModel,
          JVMPlatformSpecific,
          BuiltinSemanticsVariant.C
        )
}
