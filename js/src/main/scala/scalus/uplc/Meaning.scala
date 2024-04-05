package scalus.uplc

import scalus.builtin.JSPlatformSpecific

object Meaning {
    val defaultBuiltins =
        BuiltinsMeaning(eval.BuiltinCostModel.defaultCostModel, JSPlatformSpecific)
}
