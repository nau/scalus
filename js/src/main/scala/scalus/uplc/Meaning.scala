package scalus.uplc

import scalus.builtin.JSPlatformSpecific

object Meaning {
    val defaultBuiltins = Meaning(eval.BuiltinCostModel.defaultCostModel)
}

class Meaning(builtinCostModel: eval.BuiltinCostModel)
    extends BuiltinsMeaning(builtinCostModel)
    with JSPlatformSpecific {
    protected def log(msg: String): Unit = ()
}
