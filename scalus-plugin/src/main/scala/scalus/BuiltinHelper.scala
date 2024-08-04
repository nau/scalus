package scalus
import dotty.tools.dotc.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.*
import scalus.sir.SIR
import scalus.uplc.DefaultFun

class BuiltinHelper(using Context) {
    val BuiltinsClass = requiredModule("scalus.builtin.Builtins")

    val DefaultFunValues: Map[Symbol, DefaultFun] = DefaultFun.values
        .map(v => lowerFirst(v.toString) -> v)
        .toMap
        .map { (k, v) => BuiltinsClass.requiredMethod(k) -> v }

    def builtinFun(s: Symbol): Option[SIR.Builtin] = {
        DefaultFunValues.get(s).map(SIR.Builtin.apply)
    }

    private def lowerFirst(s: String): String =
        if s == null || s.isEmpty || !s.charAt(0).isUpper then s
        else s.updated(0, s.charAt(0).toLower)
}
