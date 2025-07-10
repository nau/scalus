package scalus.sir.lowering

import org.typelevel.paiges.Doc
import scalus.sir.*
import scalus.uplc.*

abstract class BaseRepresentationProxyLoweredValue(
    input: LoweredValue,
    override val representation: LoweredValueRepresentation,
    override val pos: SIRPosition
) extends ProxyLoweredValue(input) {

    override def sirType: SIRType = input.sirType

    override def docRef(style: PrettyPrinter.Style): Doc = {
        docDef(style)
    }
}

/** A proxy which change only the representation of the input value (without changing the underlying
  * generated code)
  */
final class RepresentationProxyLoweredValue(
    input: LoweredValue,
    override val representation: LoweredValueRepresentation,
    override val pos: SIRPosition
) extends BaseRepresentationProxyLoweredValue(input, representation, pos) {

    override def termInternal(gctx: TermGenerationContext): Term =
        input.termInternal(gctx)

    override def docDef(style: PrettyPrinter.Style): Doc = {
        val left = Doc.text("repr.proxy") + Doc.text("(")
        val right = PrettyPrinter.inBrackets(
          representation.doc
        ) + Doc.text(")")
        input.docRef(style).bracketBy(left, right)
    }

    override def docRef(style: PrettyPrinter.Style): Doc = {
        docDef(style)
    }

}

/** A proxy which changes the input value to be specific type and representation.
  */
final class TypeRepresentationProxyLoweredValue(
    input: LoweredValue,
    inSirType: SIRType,
    override val representation: LoweredValueRepresentation,
    inPos: SIRPosition
) extends ProxyLoweredValue(input) {

    override def sirType: SIRType = inSirType

    override def pos: SIRPosition = inPos

    override def termInternal(gctx: TermGenerationContext): Term =
        input.termInternal(gctx)

    override def docDef(style: PrettyPrinter.Style): Doc = {
        val left = Doc.text("cast.repr.proxy") + Doc.text("(")
        val right = Doc.text(":") + Doc.text(sirType.show) + PrettyPrinter.inBrackets(
          representation.doc
        ) + Doc.text(")")
        input.docRef(style).bracketBy(left, right)
    }

    override def docRef(style: PrettyPrinter.Style): Doc = {
        docDef(style)
    }

}
