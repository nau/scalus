package scalus

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.plugins.*
import dotty.tools.dotc.util.Spans
import dotty.tools.dotc.typer.Implicits
import dotty.tools.io.ClassPath
import scalus.flat.{DecoderState, FlatInstantces}
import scalus.flat.FlatInstantces.SIRHashConsedFlat
import scalus.sir.{RemoveRecursivity, SIR}
import scalus.utils.{HSRIdentityHashMap, HashConsed, HashConsedDecoderState, HashConsedEncoderState}

import java.nio.charset.StandardCharsets
import scala.annotation.nowarn
import scala.collection.immutable
import scala.language.implicitConversions

class Plugin extends StandardPlugin {
    val name: String = "scalus"
    override val description: String = "Compile Scala to Scalus IR"

    override def init(options: List[String]): List[PluginPhase] = {
        val debugLevel = options
            .find(_.startsWith("debugLevel="))
            .map(_.substring("debugLevel=".length))
            .map(_.toInt)
            .getOrElse(0)
        new ScalusPhase(debugLevel) :: Nil
    }
}

/** A plugin phase that compiles Scala code to Scalus Intermediate Representation (SIR).
  *
  * It's a two-phase process:
  *   1. Compile Scala code to [[SIR]] and store it in JARs
  *   1. Link SIR to the final code by replacing calls to `compile` and `compileDebug` with a string
  *      literal that contains the encoded SIR and a call to `decodeStringLatin1` that decodes it
  *      back.
  */
class ScalusPhase(debugLevel: Int) extends PluginPhase {
    import tpd.*

    val phaseName = "Scalus"

    // We need to run after the "first transform" phase to have some optimizations applied,
    // like inlining, constant folding, etc.
    override val runsAfter: Set[String] = Set("firstTransform")
    // We need to run before the "patternMatcher" phase to have the SIR available for pattern matching
    override val runsBefore: Set[String] = Set("patternMatcher")

    override def allowsImplicitSearch: Boolean = true

    /** Compiles the current compilation unit to SIR and stores it in JARs as .sir file.
      */
    override def prepareForUnit(tree: Tree)(using Context): Context = {
        // bug in dotty: sometimes we called with the wrong phase in context
        if summon[Context].phase != this then
            prepareForUnit(tree)(using summon[Context].withPhase(this))
        else
            if debugLevel > 0 then report.echo(s"Scalus: ${ctx.compilationUnit.source.file.name}")
            val options = retrieveCompilerOptions(tree, debugLevel > 0)
            val sirLoader = createSirLoader
            val compiler = new SIRCompiler(sirLoader, options)
            compiler.compileModule(tree)
            ctx
    }

    override def prepareForApply(tree: tpd.Apply)(using Context): Context = {
        // bug in dotty: sometimes we called with the wrong phase in context. set phase themself.
        if summon[Context].phase != this then ctx.withPhase(this)
        else ctx
    }

    /** Replaces calls to `compile` and `compileDebug` with a fully linked Flat-encoded [[SIR]]
      * representation.
      */
    override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree =
        val compilerModule = requiredModule("scalus.Compiler")
        val compileSymbol = compilerModule.requiredMethod("compile")
        val compileDebugSymbol = compilerModule.requiredMethod("compileDebug")
        val isCompileDebug = tree.fun.symbol == compileDebugSymbol

        if tree.fun.symbol == compileSymbol || isCompileDebug then
            // report.echo(tree.showIndented(2))
            val options = retrieveCompilerOptions(tree, isCompileDebug)

            val localDebugLevel =
                if options.debugLevel == 0 && debugLevel == 0 && isCompileDebug then 10
                else if options.debugLevel > debugLevel then options.debugLevel
                else debugLevel

            val code = tree.args.head
            val sirLoader = createSirLoader
            val compiler = new SIRCompiler(sirLoader, options)
            val start = System.currentTimeMillis()
            val result =
                val result = compiler.compileToSIR(code, isCompileDebug)
                val linked = SIRLinker(
                  SIRLinkerOptions(options.universalDataRepresentation, localDebugLevel),
                  sirLoader
                )
                    .link(result, tree.srcPos)
                RemoveRecursivity(linked)

            if isCompileDebug then
                val time = System.currentTimeMillis() - start
                report.echo(
                  s"Scalus compileDebug at ${tree.srcPos.sourcePos.source}:${tree.srcPos.line} in $time ms, options=${options}"
                )

            convertSIRToTree(result, code, tree.span, isCompileDebug)
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
    private def convertSIRToTree(
        sir: SIR,
        origin: Tree,
        span: Spans.Span,
        debug: Boolean
    )(using Context): Tree = {
        val bitSize = SIRHashConsedFlat.bitSizeHC(sir, HashConsed.State.empty)
        val byteSize = ((bitSize + 1 /* for filler */ ) / 8) + 1 /* minimum size */
        val encodedState = HashConsedEncoderState.withSize(byteSize)
        SIRHashConsedFlat.encodeHC(sir, encodedState)
        encodedState.encode.filler()
        val bytes = encodedState.encode.result
        if debug then
            // try to decode the SIR back
            val decodeState = HashConsedDecoderState(DecoderState(bytes), HashConsed.State.empty)
            val decodedRef = SIRHashConsedFlat.decodeHC(decodeState)
            decodeState.runFinCallbacks()
            @nowarn val sirRer: SIR =
                decodedRef.finValue(decodeState.hashConsed, 0, new HSRIdentityHashMap)
            report.echo("ScalusPhace.convertSIRToTree: sir was decoded back successfully")

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

            We split the bytes into chunks of 65000 bytes, because the maximum size of a String literal is 65535 bytes.
            https://stackoverflow.com/questions/816142/strings-maximum-length-in-java-calling-length-method
            https://asm.ow2.io/javadoc/org/objectweb/asm/ByteVector.html#putUTF8(java.lang.String)

            But for some reason, it's not possible to create a string literal with 65535 bytes.
            45000 is a safe value that works.
         */
        val strings = (
          for bytes <- bytes.grouped(45000)
          yield
              val str = new String(bytes, StandardCharsets.ISO_8859_1)
              Literal(Constant(str)).withSpan(span): Tree
        ).toList
        // Concatenate all the strings: "str1" + "str2" + ...
        val concatenatedStrings =
            strings.reduce((lhs, rhs) => lhs.select(nme.Plus).appliedTo(rhs).withSpan(span))
        if debug then {
            // save the SIR to a file for debugging purposes
            val groupedBytes = bytes.grouped(45000).toList
            val strings = groupedBytes.map { b =>
                Literal(Constant(new String(b, StandardCharsets.ISO_8859_1))).withSpan(span)
            }
            val parts = strings.map {
                case Literal(Constant(str: String)) => str
                case _ => throw new RuntimeException("Expected a string literal")
            }
            val codedStr = parts.mkString
            report.echo(s"string.length: ${codedStr.length},  nParts: ${parts.size}")
            val path = ctx.settings.outputDir.value.file.toPath
                .resolve(s"${ctx.compilationUnit.source.file.name}_${span.start}.sir")
            import java.nio.file.Files
            Files.createDirectories(path.getParent)
            Files.write(path, codedStr.getBytes(StandardCharsets.ISO_8859_1))
            report.echo(s"Scalus: saved SIR to ${path}")
            report.echo(s"Scalus: SIR size: ${codedStr.length} characters, ${bitSize} bits")
        }
        // // Generate scalus.sir.ToExprHSSIRFlat.decodeStringLatin1(str1 + str2 + ...)
        val sirToExprFlat = requiredModule("scalus.sir.ToExprHSSIRFlat")
        val decodeLatin1SIR = sirToExprFlat.requiredMethod("decodeStringLatin1")
        ref(sirToExprFlat).select(decodeLatin1SIR).appliedTo(concatenatedStrings).withSpan(span)
    }

    private def createSirLoader(using Context): SIRLoader = {
        new SIRLoader(
          SIRLoaderOptions(
            ClassPath.expandPath(ctx.settings.classpath.value, expandStar = true),
            Some(ctx.settings.outputDir.value.toURL),
            Some("./shared/src/main/resources/predefined-sirs/")
          )
        )
    }

    private def retrieveCompilerOptions(
        posTree: Tree,
        isCompilerDebug: Boolean
    )(using Context): SIRCompilerOptions = {

        // Default options
        var backend: String = "SimpleSirToUplcLowering"
        var generateErrorTraces: Boolean = true
        var optimizeUplc: Boolean = true
        var debug: Boolean = false
        var codeDebugLevel: Int = 0
        var useUniversalDataConversion: Boolean = false

        def parseTargetLoweringBackend(value: Tree): Unit = {
            var parsed = true
            value match {
                case Ident(name) =>
                    backend = name.toString
                case Select(_, name) =>
                    backend = name.toString
                case _ =>
                    parsed = false
                    report.warning(
                      s"ScalusPhase: Expected an identifier or select expression for targetLoweringBackend, but found: ${value.show}",
                      value.srcPos
                    )
            }
            if backend.equals("SirToUplcV3Lowering") then useUniversalDataConversion = true
            if parsed then
                if debugLevel > 0 || isCompilerDebug then
                    println(
                      s"parseTargetLoweringBackend: ${backend}, useUniversalDataConversion=${useUniversalDataConversion}"
                    )
            else {
                report.warning(
                  s"ScalusPhase: Failed to parse targetLoweringBackend, using default: ${backend}",
                  value.srcPos
                )
            }
        }

        def parseBooleanValue(value: Tree): Boolean = {
            value match {
                case Literal(Constant(flag: Boolean)) => flag
                case _ =>
                    report.warning(
                      s"ScalusPhase: Expected a boolean literal, but found: ${value.show}\ntree:${value}",
                      value.srcPos
                    )
                    false
            }
        }

        def parseIntValue(value: Tree): Int = {
            value match {
                case Literal(Constant(num: Int)) => num
                case _ =>
                    report.warning(
                      s"ScalusPhase: Expected an integer literal, but found: ${value.show}",
                      posTree.srcPos.startPos
                    )
                    0 // default value if parsing fails
            }
        }

        def parseArg(arg: Tree, idx: Int): Unit = {
            arg match
                case tpd.NamedArg(name, value) =>
                    if name.toString == "targetLoweringBackend" then
                        parseTargetLoweringBackend(value)
                    else if name.toString == "generateErrorTraces" then
                        generateErrorTraces = parseBooleanValue(value)
                    else if name.toString == "optimizeUplc" then
                        optimizeUplc = parseBooleanValue(value)
                    else if name.toString == "debug" then debug = parseBooleanValue(value)
                    else if name.toString == "debugLevel" then codeDebugLevel = parseIntValue(value)
                    else if name.toString == "useUniversalDataConversion" then
                        useUniversalDataConversion = parseBooleanValue(value)
                    else {
                        report.warning(
                          s"ScalusPhase: Unknown compiler option: $name",
                          posTree.srcPos.startPos
                        )
                    }
                case value =>
                    idx match
                        case 0 => // targetLoweringBackend
                            parseTargetLoweringBackend(value)
                        case 1 => // generateErrorTraces
                            generateErrorTraces = parseBooleanValue(value)
                        case 2 => // optimizeUplc
                            optimizeUplc = parseBooleanValue(value)
                        case 3 => // debug
                            debug = parseBooleanValue(value)
                            if debug then codeDebugLevel = 10
                        case _ =>
                            report.warning(
                              s"ScalusPhase: too many position argiments for scalus.compiler.Options, expected max 4, but found ${idx + 1}",
                              posTree.srcPos.startPos
                            )
        }

        val compilerOptionType = requiredClassRef("scalus.Compiler.Options")
        if !ctx.phase.allowsImplicitSearch then
            println(
              s"ScalusPhase: Implicit search is not allowed in phase ${ctx.phase.phaseName}. "
            )
        summon[Context].typer.inferImplicit(
          compilerOptionType,
          EmptyTree,
          posTree.span
        ) match {
            case Implicits.SearchSuccess(tree, ref, level, isExtension) =>
                // report.echo(s"Found compiler options: ${tree.show}")
                // report.echo(s"deftree=${tree.symbol.defTree.show}")
                val deftree = tree.symbol.defTree
                if deftree.isEmpty then
                    report.warning(
                      s"CompilerOptions found but deftree is empty, compiler options may be incomplete",
                      posTree.srcPos
                    )
                else if deftree.isInstanceOf[tpd.DefDef] then
                    val defDefTree = deftree.asInstanceOf[tpd.DefDef]
                    val underTyped = defDefTree.rhs match
                        case tpd.Typed(obj, tp) =>
                            obj
                        case _ => defDefTree
                    underTyped match
                        case tpd.Apply(obj, args) =>
                            // report.echo(s"defDefTree.rhs.apply, args=${args}")
                            if obj.symbol != Symbols.requiredMethod(
                                  "scalus.Compiler.Options.apply"
                                )
                            then
                                report.warning(
                                  "expected scalus.Compiler.Options.apply",
                                  posTree.srcPos
                                )
                            args.zipWithIndex.foreach { case (arg, idx) =>
                                parseArg(arg, idx)
                            }
                        case _ =>
                            report.warning(
                              s"ScalusPhase: Expected a call to scalus.Compiler.Options.apply, but found: ${underTyped.show}",
                              posTree.srcPos
                            )
                else report.warning("defdef expected as compiler options", deftree.srcPos)
            case failure @ Implicits.SearchFailure(_) =>
                if isCompilerDebug && debugLevel > 20 then {
                    report.warning(
                      s"ScalusPhase: No compiler options found, using default options",
                      posTree.srcPos.startPos
                    )
                    report.warning(s"search result: ${failure.show}")
                } else {
                    // report.echo("No compiler options found, using default options")
                }
        }
        val retval = SIRCompilerOptions(
          useSubmodules = false,
          universalDataRepresentation = useUniversalDataConversion,
          debugLevel = if debugLevel == 0 then codeDebugLevel else debugLevel
        )
        if isCompilerDebug then
            println(
              s"retrieveCompilerOptions, retval=${retval}, useUniversalDataConversion=${useUniversalDataConversion}"
            )
        retval
    }

}
