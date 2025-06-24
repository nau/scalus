package scalus.sir.lowering.typegens

import scalus.sir.*
import scalus.sir.lowering.*
import scalus.uplc.*

class RepresentationProxyLoweredValue(
    input: LoweredValue,
    val representation: LoweredValueRepresentation,
    val pos: SIRPosition
) extends LoweredValue {

    override def sirType: SIRType = input.sirType

    override def dominatingUplevelVars: Set[IdentifiableLoweredValue] = {
        input.dominatingUplevelVars
    }

    override def usedUplevelVars: Set[IdentifiableLoweredValue] = {
        input.usedUplevelVars
    }

    override def addDependent(value: IdentifiableLoweredValue): Unit = {
        input.addDependent(value)
    }

    def termInternal(gctx: TermGenerationContext): Term =
        input.termInternal(gctx)

    def show: String = {
        s"RepresentationProxyLoweredValue(${input.show}, $representation)"
    }

}
