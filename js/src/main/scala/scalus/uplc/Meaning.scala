package scalus.uplc

import scalus.builtin.JSPlatformSpecific

object Meaning {
    val defaultBuiltins = Meaning(eval.BuiltinCostModel.defaultCostModel)
}

class Meaning(builtinCostModel: eval.BuiltinCostModel)
    extends BuiltinsMeaning(builtinCostModel, JSPlatformSpecific) {
    protected def log(msg: String): Unit = ()
}
