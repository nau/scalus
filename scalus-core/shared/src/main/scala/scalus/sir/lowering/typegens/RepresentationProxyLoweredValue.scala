package scalus.sir.lowering.typegens

import org.typelevel.paiges.Doc
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

    override def docDef(style: PrettyPrinter.Style): Doc = {
        val left = Doc.text("repr.proxy") + Doc.text("(")
        val right = Doc.text(":") + Doc.text(sirType.show) + PrettyPrinter.inBrackets(
          representation.doc
        ) + Doc.text(")")
        input.docRef(style).bracketBy(left, right)
    }

    override def docRef(style: PrettyPrinter.Style): Doc = {
        docDef(style)
    }

}
