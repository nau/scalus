package scalus.sir.lowering.typegens

import scalus.sir.*
import scalus.sir.lowering.*
import scalus.uplc.*

trait RepresentationProxyLoweredValue(
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

    def termInternal(gctx: TermGenerationContext): Term

    def show: String = {
        s"RepresentationProxyLoweredValue(${input.show}, $representation)"
    }

}
