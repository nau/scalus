package scalus.uplc

import scalus.builtin.JVMPlatformSpecific

object Meaning {
    val defaultBuiltins = Meaning(eval.BuiltinCostModel.defaultCostModel)
}

class Meaning(builtinCostModel: eval.BuiltinCostModel)
    extends BuiltinsMeaning(builtinCostModel, JVMPlatformSpecific) {
    protected def log(msg: String): Unit = ()
}
