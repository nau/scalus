package scalus

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.DenotTransformers.IdentityDenotTransformer
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Denotations.*
import dotty.tools.dotc.plugins.*
import dotty.tools.dotc.transform.{ExpandSAMs, Pickler, PostTyper}
import dotty.tools.dotc.util.Spans
import dotty.tools.dotc.typer.Implicits
import dotty.tools.io.ClassPath
import scalus.flat.{DecoderState, FlatInstantces}
import scalus.flat.FlatInstantces.SIRHashConsedFlat
import scalus.sir.{RemoveRecursivity, SIR}

import java.nio.charset.StandardCharsets
import scala.annotation.nowarn
import scala.collection.immutable
import scala.language.implicitConversions

class Plugin extends StandardPlugin {
    val name: String = "scalus"
    override val description: String = "Compile Scala to Scalus IR"

    // val compiledSirs: mutable.Map[String, SIR] = mmutable.Map.empty

    override def init(options: List[String]): List[PluginPhase] = {
        val debugLevel = options
            .find(_.startsWith("debugLevel="))
            .map(_.substring("debugLevel=".length))
            .map(_.toInt)
            .getOrElse(0)
        new ScalusPreparePhase(debugLevel) :: ScalusPhase(debugLevel) :: Nil
    }
}

object Plugin {

    // TODO: check that we have no variabl with such name in the source code
    val SIR_MODULE_VAL_NAME = "sirModule"
    val SIR_DEPS_VAL_NAME = "sirDeps"

}

/** A prepare phase which should run before pickling to create additional variables in objects,
  * where SIR will be stored.
  */
class ScalusPreparePhase(debugLevel: Int) extends PluginPhase with IdentityDenotTransformer {

    val phaseName = "ScalusPrepare"

    // We need to run before the "pickler" phase to have SIR available for pickling
    override val runsAfter: Set[String] = Set(PostTyper.name)
    override val runsBefore: Set[String] = Set(Pickler.name)

    override def changesMembers: Boolean = true

    override def prepareForTypeDef(tree: tpd.TypeDef)(using Context): Context = {
        // bug in dotty: sometimes we called with the wrong phase in context
        if summon[Context].phase != this then
            prepareForTypeDef(tree)(using summon[Context].withPhase(this))
        else ctx
    }

    override def transformTypeDef(tree: tpd.TypeDef)(using Context): tpd.Tree = {
        val compileAnnot = requiredClassRef("scalus.Compile").symbol.asClass
        if tree.symbol.hasAnnotation(compileAnnot) && tree.symbol.is(Flags.Module) then
            val preprocessor = new SIRPreprocessor(this, debugLevel)
            preprocessor.transformTypeDef(tree)
        else tree
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
        try
            if summon[Context].phase != this then
                prepareForUnit(tree)(using summon[Context].withPhase(this))
            else
                if debugLevel > 0 then
                    report.echo(s"Scalus: ${ctx.compilationUnit.source.file.name}")
                val options = retrieveCompilerOptions(tree, debugLevel > 0)
                val sirLoader = createSirLoader
                val compiler = new SIRCompiler(sirLoader, options)
                compiler.compileModule(tree)
                ctx
        catch
            case scala.util.control.NonFatal(ex) =>
                ex.printStackTrace()
                throw ex
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
        try
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

                convertFlatToTree(
                  result,
                  SIRHashConsedFlat,
                  requiredModule("scalus.sir.ToExprHSSIRFlat"),
                  tree.span,
                  isCompileDebug
                )
            else tree
        catch
            case scala.util.control.NonFatal(ex) =>
                ex.printStackTrace()
                throw ex
    end transformApply

    override def transformTypeDef(tree: tpd.TypeDef)(using Context): tpd.Tree =
        // If the template has a compile annotation, we need to add a variable for SIR
        val compileAnnot = requiredClassRef("scalus.Compile").symbol.asClass
        if tree.symbol.hasAnnotation(compileAnnot) && tree.symbol.is(Flags.Module) then {
            val sirBodyAnnotation =
                requiredClass("scalus.sir.SIRBodyAnnotation")
            tree.symbol.getAnnotation(sirBodyAnnotation) match
                case Some(annotation) =>
                    val moduleSIR = annotation.arguments.head
                    val depsMap = annotation.arguments.tail.head
                    tree.rhs match
                        case template: tpd.Template =>
                            // Add a variable for SIR in the template
                            val newBody = template.body.map { e =>
                                e match {
                                    case vd: ValDef
                                        if vd.symbol.name.toString == Plugin.SIR_MODULE_VAL_NAME =>
                                        cpy.ValDef(vd)(rhs = moduleSIR)
                                    case vd: ValDef
                                        if vd.symbol.name.toString == Plugin.SIR_DEPS_VAL_NAME =>
                                        // If the variable already exists, we can skip it
                                        cpy.ValDef(vd)(rhs = depsMap)
                                    case _ =>
                                        e
                                }
                            }
                            val newTemplate = cpy.Template(template)(body = newBody)
                            cpy.TypeDef(tree)(rhs = newTemplate)
                        case _ =>
                            report.warning(
                              s"ScalusPhase: Expected a template for ${tree.symbol.fullName}, but found: ${tree.rhs.show}",
                              tree.srcPos
                            )
                            tree
                case None =>
                    println(
                      s"ScalusPhase: SIRBodyAnnotation not found for ${tree.symbol.fullName}"
                    )
                    tree
        } else tree

    private def createSirLoader(using Context): SIRLoader = {
        new SIRLoader(
          SIRLoaderOptions(
            ClassPath.expandPath(ctx.settings.classpath.value, expandStar = true),
            Option(ctx.settings.outputDir.value.toURL),
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
