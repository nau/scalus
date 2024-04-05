package scalus.uplc

import scalus.builtin.JVMPlatformSpecific

object Meaning {
    val defaultBuiltins =
        BuiltinsMeaning(eval.BuiltinCostModel.defaultCostModel, JVMPlatformSpecific)
}
