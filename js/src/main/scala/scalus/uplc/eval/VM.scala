package scalus.uplc
package eval

import scalus.builtin.*

object VM extends VMBase {
    def evaluateTerm(params: MachineParams, term: Term): CekResult = {
        val cek = new CekMachine(params)
        cek.runCek(term)
    }
}

final class CekMachine(params: MachineParams)
    extends AbstractCekMachine(params)
    with JSPlatformSpecific

@deprecated("Use VM instead", "0.7.0")
object Cek {
    @deprecated("Use VM methods instead", "0.7.0")
    def evalUPLC(term: Term)(using ps: PlatformSpecific): Term = {
        val params = MachineParams.defaultParams
        val debruijnedTerm = DeBruijn.deBruijnTerm(term)
        new CekMachine(params).evaluateTerm(debruijnedTerm)
    }

    def evalUPLCProgram(p: Program)(using ps: PlatformSpecific): Term = evalUPLC(p.term)
}
