package scalus.uplc

import scalus.builtin.NodeJsPlatformSpecific

object Meaning {
    val defaultBuiltins =
        BuiltinsMeaning(eval.BuiltinCostModel.defaultCostModel, NodeJsPlatformSpecific)
}
