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
import java.nio.charset.StandardCharsets

import scala.language.implicitConversions

class SIRConverter(using Context) {
    import tpd.*

    private def convertViaSerialization(sir: SIR, span: Spans.Span): Tree = {
        val bitSize =
            scalus.flat.FlatInstantces.SIRHashConsedFlat.bitSizeHC(sir, HashConsed.State.empty)
        val byteSize = (bitSize + 1 /* for filler */ / 8) + 1 /* minimum size */
        val encodedState = HashConsedEncoderState.withSize(byteSize)
        FlatInstantces.SIRHashConsedFlat.encodeHC(sir, encodedState)
        encodedState.encode.filler()
        val bytes = encodedState.encode.result
        /*
            We could generate Array[Byte] constant from bytes directly, like this:

            val bytesLiterals = bytes.map(b => Literal(Constant(b))).toList
            JavaSeqLiteral(bytesLiterals, TypeTree(defn.ByteType))

            But Scala 3.3.4 generates the array literal inside a method.
            That sometimes produces "Method too large" error. JVM has a limit of 64KB for a method.
            But for String's it appears to generate a `LDC` opcode loading the String from a constant pool.
            So we convert the bytes to a String in ISO_8859_1 encoding to get a one byte per character.
            It was Base64 encoded before, but it's 33% larger than the original bytes.
            We could fit two bytes in one character, but then it's not a valid UTF-16 string.
         */
        val str = new String(bytes, StandardCharsets.ISO_8859_1)
        val stringLiteral = Literal(Constant(str)).withSpan(span)
        val sirToExprFlat = requiredModule("scalus.sir.ToExprHSSIRFlat")
        val decodeLatin1SIR = sirToExprFlat.requiredMethod("decodeStringLatin1")
        ref(sirToExprFlat).select(decodeLatin1SIR).appliedTo(stringLiteral).withSpan(span)
    }

    def convertSIRToTree(sir: SIR, span: Spans.Span): Tree = {
        val res = convertViaSerialization(sir, span)
        // println(res.showIndented(2))
        res
    }
}
