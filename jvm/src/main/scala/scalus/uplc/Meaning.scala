package scalus.uplc

import scalus.builtin.JVMPlatformSpecific

object Meaning {
    val defaultBuiltins = Meaning(eval.BuiltinCostModel.default)
}

class Meaning(builtinCostModel: eval.BuiltinCostModel) extends BuitlinsMeaning(builtinCostModel) with JVMPlatformSpecific