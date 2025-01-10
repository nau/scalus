package scalus

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.plugins.*
import dotty.tools.dotc.util.Spans
import scalus.flat.FlatInstantces
import scalus.sir.SIR
import scalus.utils.HashConsed
import scalus.utils.HashConsedEncoderState

import java.nio.charset.StandardCharsets
import scala.collection.immutable
import scala.language.implicitConversions
import scala.util.control.NonFatal

enum Mode:
    case Compile, Link

class Plugin extends StandardPlugin {
    val name: String = "scalus"
    override val description: String = "Compile Scala to Scalus IR"

    override def init(options: List[String]): List[PluginPhase] =
        new ScalusPhase :: Nil
}

/** A plugin phase that compiles Scala code to Scalus Intermediate Representation (SIR).
  *
  * It's a two-phase process:
  *   1. Compile Scala code to [[SIR]] and store it in JARs
  *   1. Link SIR to the final code by replacing calls to `compile` and `compileDebug` with a string
  *      literal that contains the encoded SIR and a call to `decodeStringLatin1` that decodes it
  *      back.
  */
class ScalusPhase extends PluginPhase {
    import tpd.*

    val phaseName = "Scalus"

    // We need to run after the "first transform" phase to have some optimizations applied,
    // like inlining, constant folding, etc.
    override val runsAfter: Set[String] = Set("firstTransform")
    // We need to run before the "patternMatcher" phase to have the SIR available for pattern matching
    override val runsBefore: Set[String] = Set("patternMatcher")

    /** Compiles the current compilation unit to SIR and stores it in JARs as .sir file.
      */
    override def prepareForUnit(tree: Tree)(using Context): Context =
        // report.echo(s"Scalus: ${ctx.compilationUnit.source.file.name}")
        val compiler =
            try new SIRCompiler(Mode.Compile)
            catch
                case NonFatal(e) =>
                    report.error(s"Failed to initialize Scalus compiler: ${e.getMessage}")
                    e.printStackTrace()
                    throw e
        compiler.compileModule(tree)
        ctx

    /** Replaces calls to `compile` and `compileDebug` with a fully linked Flat-encoded [[SIR]]
      * representation.
      */
    override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree =
        val compilerModule = requiredModule("scalus.Compiler")
        val compileSymbol = compilerModule.requiredMethod("compile")
        val compileDebugSymbol = compilerModule.requiredMethod("compileDebug")
        if tree.fun.symbol == compileSymbol || tree.fun.symbol == compileDebugSymbol then
            // report.echo(tree.showIndented(2))
            val code = tree.args.head
            val compiler = new SIRCompiler(Mode.Link)
            val result = compiler.compileToSIRAndLink(code, tree.fun.symbol == compileDebugSymbol)
            convertSIRToTree(result, tree.span)
        else tree
    end transformApply

    /** Convert SIR to a [[Tree]] that represents that SIR by encoding it to a string and generating
      * a code that decodes it back.
      *
      * So a call to `compile` or `compileDebug` will be replaced with a string literal that
      * contains the encoded SIR and a call to `decodeStringLatin1` that will decode it back.
      *
      * {{{
      *   val sir = compile(true)
      * }}}
      * becomes
      * {{{
      *  val sir = decodeStringLatin1("...encoded SIR...")
      *  // that decodes to SIR.Const(true)
      * }}}
      * This is a bit of a hack. Otherwise, we need to convert every SIR node to a [[Tree]] manually
      * by something like
      * {{{
      *   val sirVar = requiredModule("scalus.sir.SIR.Var").requiredMethod("apply")
      *   val varTree = ref(sirVar).appliedTo(arg)
      *   ...
      * }}}
      * which is a lot of boilerplate. And we have [[Flat]] encoding for SIR, so we can use it.
      */
    private def convertSIRToTree(sir: SIR, span: Spans.Span)(using Context): Tree = {
        val res = {
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
        // println(res.showIndented(2))
        res
    }
}
