package scalus.uplc
package eval

import scalus.builtin.*

@deprecated("Use PlutusVM instead", "0.8.0")
object VM extends PlutusVMBase(NativePlatformSpecific)

@deprecated("Use PlutusVM instead", "0.7.0")
object Cek {
    @deprecated("Use VM methods instead", "0.7.0")
    def evalUPLC(term: Term): Term = VM.evaluateTerm(term)

    def evalUPLCProgram(p: Program): Term = VM.evaluateProgram(p)
}
