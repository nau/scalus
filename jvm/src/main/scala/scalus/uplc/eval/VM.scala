package scalus.uplc
package eval

import scalus.builtin.*

object VM extends PlutusVM(JVMPlatformSpecific)

@deprecated("Use VM instead", "0.7.0")
object Cek {
    @deprecated("Use VM methods instead", "0.7.0")
    def evalUPLC(term: Term): Term = VM.evaluateTerm(term)

    def evalUPLCProgram(p: Program): Term = VM.evaluateProgram(p)
}
