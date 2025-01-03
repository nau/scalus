package scalus

import dotty.tools.dotc.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.*
import scalus.sir.SIR
import scalus.sir.SIRBuiltins

class BuiltinHelper(using ctx: Context) {

    private val DefaultFunSIRBuiltins: Map[Symbol, SIR.Builtin] = Macros.generateBuiltinsMap(ctx)

    def builtinFun(s: Symbol): Option[SIR.Builtin] = {
        DefaultFunSIRBuiltins.get(s)
    }
}
