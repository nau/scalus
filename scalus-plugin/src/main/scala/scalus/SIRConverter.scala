package scalus

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.util.Spans
import scalus.flat.FlatInstantces
import scalus.sir.SIR
import scalus.utils.HashConsed
import scalus.utils.HashConsedEncoderState

import scala.language.implicitConversions

class SIRConverter(using Context) {
    import tpd.*

    private def convertViaSerialization(sir: SIR, span: Spans.Span): Tree = {
        val bitSize =
            scalus.flat.FlatInstantces.SIRHashConsedFlat.bitSizeHC(sir, HashConsed.State.empty)
        val encodedState = HashConsedEncoderState.withSize(bitSize)
        FlatInstantces.SIRHashConsedFlat.encodeHC(sir, encodedState)
        encodedState.encode.filler()
        val bytes = encodedState.encode.result
        val base64 = java.util.Base64.getEncoder.encodeToString(bytes)
        val stringLiteral = Literal(Constant(base64)).withSpan(span)
        val sirToExprFlat = requiredModule("scalus.sir.ToExprHSSIRFlat")
        val decodeBase64SIR = sirToExprFlat.requiredMethod("decodeBase64")
        ref(sirToExprFlat).select(decodeBase64SIR).appliedTo(stringLiteral).withSpan(span)
    }

    def convertSIRToTree(sir: SIR, span: Spans.Span): Tree = {
        val res = convertViaSerialization(sir, span)
        // println(res.showIndented(2))
        res
    }
}
