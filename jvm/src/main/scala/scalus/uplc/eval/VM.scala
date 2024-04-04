package scalus.uplc
package eval

import scalus.builtin.*

object VM extends VMBase(JVMPlatformSpecific)

@deprecated("Use VM instead", "0.7.0")
object Cek {
    @deprecated("Use VM methods instead", "0.7.0")
    def evalUPLC(term: Term): Term = {
        val params = MachineParams.defaultParams
        val debruijnedTerm = DeBruijn.deBruijnTerm(term)
        new CekMachine(params, NoBudgetSpender, JVMPlatformSpecific).evaluateTerm(debruijnedTerm)
    }

    @deprecated("Use VM methods instead", "0.7.0")
    def evalUPLCProgram(p: Program): Term = evalUPLC(p.term)
}
