package scalus

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.DenotTransformers.IdentityDenotTransformer
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.plugins.*
import dotty.tools.dotc.transform.{ElimByName, ExpandSAMs, Pickler, PostTyper}
import dotty.tools.dotc.util.{Spans, SrcPos}
import dotty.tools.dotc.typer.Implicits
import dotty.tools.io.ClassPath
import scalus.flat.FlatInstantces
import scalus.flat.FlatInstantces.SIRHashConsedFlat
import scalus.sir.{RemoveRecursivity, SIR, SIRDefaultOptions, SIRPosition}

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
    override val runsBefore: Set[String] = Set("patternMatcher", ElimByName.name)

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
                val sirResult =
                    val sirOnly = compiler.compileToSIR(code, isCompileDebug)
                    if options.linkInRuntime then sirOnly
                    else
                        val linked = SIRLinker(
                          SIRLinkerOptions(options.universalDataRepresentation, localDebugLevel),
                          sirLoader
                        ).link(sirOnly, tree.srcPos)
                        RemoveRecursivity(linked)

                if isCompileDebug then
                    val time = System.currentTimeMillis() - start
                    report.echo(
                      s"Scalus compileDebug at ${tree.srcPos.sourcePos.source}:${tree.srcPos.line} in $time ms, options=${options}"
                    )

                val flatTree = convertFlatToTree(
                  sirResult,
                  SIRHashConsedFlat,
                  requiredModule("scalus.sir.ToExprHSSIRFlat"),
                  tree.span,
                  isCompileDebug
                )
                val result =
                    if options.linkInRuntime then
                        val myModuleName = s"${tree.srcPos.sourcePos.source}:${tree.srcPos.line}"
                        val dependencyEntries = compiler.gatherExternalModulesFromSir(
                          myModuleName,
                          sirResult,
                          Map.empty
                        )
                        val depsTree =
                            try compiler.buildDepsTree(myModuleName, dependencyEntries, tree.srcPos)
                            catch
                                case scala.util.control.NonFatal(ex) =>
                                    println(
                                      "Error building deps tree,  myModuleName=" + myModuleName
                                    )
                                    throw ex;
                        val SIRLinkerModule = requiredModule("scalus.sir.linking.SIRLinker")
                        val SIRLinkerMethod = SIRLinkerModule.requiredMethod("link")
                        val sirPos =
                            createSIRPositionTree(SIRPosition.fromSrcPos(tree.srcPos), tree.span)
                        val linkerOptionsTree = createLinkerOptionsTree(options, tree.srcPos)
                        val sirLinkerCall = tpd
                            .ref(SIRLinkerMethod)
                            .appliedTo(
                              flatTree,
                              sirPos,
                              depsTree,
                              linkerOptionsTree
                            )
                            .withSpan(tree.span)
                        sirLinkerCall
                    else flatTree
                result
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
        var backend: String = SIRDefaultOptions.targetLoweringBackend.toString
        var generateErrorTraces: Boolean = SIRDefaultOptions.generateErrorTraces
        var optimizeUplc: Boolean = SIRDefaultOptions.optimizeUplc
        var debug: Boolean = false
        var codeDebugLevel: Int = 0
        var useUniversalDataConversion: Boolean = false
        var runtimeLinker: Boolean = SIRDefaultOptions.runtimeLinker
        var writeSIRToFile: Boolean = SIRDefaultOptions.writeSIRToFile

        def parseTargetLoweringBackend(value: Tree, vals: List[Tree]): Unit = {
            var parsed = true
            value match {
                case Ident(name) =>
                    if name.startsWith("$lessinit$greater$default")
                    then
                        // This is a default value for a parameter,
                        backend = SIRDefaultOptions.targetLoweringBackend.toString
                    else if name.toString.startsWith("targetLoweringBackend$")
                    then
                        // This is a default value for a parameter,
                        findValdefWithName(name, vals) match
                            case Some(vd) =>
                                parseTargetLoweringBackend(vd.rhs, Nil)
                            case None =>
                                report.warning(
                                  s"ScalusPhase: Expected a value definition for ${name}, but not found in the template",
                                  value.srcPos
                                )
                    else backend = name.toString
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
            if parsed then {
                // check that the backend is valid
                if !(backend == "SimpleSirToUplcLowering" || backend == "SirToUplc110Lowering" || backend == "SirToUplcV3Lowering")
                then
                    report.warning(
                      s"ScalusPhase: Unknown targetLoweringBackend: ${backend}, using default: ${SIRDefaultOptions.targetLoweringBackend}",
                      value.srcPos
                    )
                    backend = SIRDefaultOptions.targetLoweringBackend.toString
                if debugLevel > 0 || isCompilerDebug then
                    println(
                      s"parseTargetLoweringBackend: ${backend}, useUniversalDataConversion=${useUniversalDataConversion}"
                    )
            } else {
                report.warning(
                  s"ScalusPhase: Failed to parse targetLoweringBackend, using default: ${backend}",
                  value.srcPos
                )
            }
        }

        def parseBooleanValue(
            paramName: String,
            value: Tree,
            vals: List[Tree]
        ): Option[Boolean] = {
            value match {
                case Literal(Constant(flag: Boolean)) => Some(flag)
                //  tree:Select(Select(Select(Ident(scalus),Compiler),Options),$lessinit$greater$default$2)
                case Select(_, name) if name.toString.startsWith("$lessinit$greater$default") =>
                    // This is a default value for a parameter,
                    None
                case Ident(name) =>
                    if name.startsWith("$lessinit$greater$default")
                    then None
                    else if name.toString.startsWith(paramName + "$") then {
                        findValdefWithName(name, vals) match
                            case Some(vd) =>
                                parseBooleanValue(paramName, vd.rhs, Nil)
                            case None =>
                                report.warning(
                                  s"ScalusPhase: Expected a value definition for ${name}, but not found in the template\n"
                                      + (if vals.isEmpty then "vals is empty"
                                         else "vals: " + vals.map(_.show).mkString(", ")),
                                  value.srcPos
                                )
                                None
                    } else {
                        val expectedParam = paramName + "$1"
                        report.warning(
                          s"ScalusPhase: Expected a boolean literal, but found identifier: ${value.show}, name=${name}, paramName=$paramName, exp=${expectedParam}",
                          value.srcPos
                        )
                        throw new RuntimeException("QQQ")
                        None
                    }
                case _ =>
                    report.warning(
                      s"ScalusPhase: Expected a boolean literal, but found: ${value.show}\ntree:${value}",
                      value.srcPos
                    )
                    None
            }
        }

        def parseIntValue(paramName: String, value: Tree, vals: List[Tree]): Option[Int] = {
            value match {
                case Literal(Constant(num: Int)) => Some(num)
                case Select(_, name)
                    if name.toString.startsWith("$lessinit$greater$default") ||
                        name.toString.startsWith(paramName + "$") =>
                    None
                case Ident(name) =>
                    if name.startsWith("$lessinit$greater$default")
                    then None
                    else if name.toString.startsWith(paramName + "$") then
                        findValdefWithName(name, vals) match
                            case Some(vd) =>
                                parseIntValue(paramName, vd.rhs, Nil)
                            case None =>
                                report.warning(
                                  s"ScalusPhase: Expected a value definition for ${name}, but not found in the template\n"
                                      + (if vals.isEmpty then "(empty vals)"
                                         else
                                             "vals: " + vals
                                                 .map(_.show)
                                                 .mkString(", ")),
                                  value.srcPos
                                )
                                None
                    else {
                        report.warning(
                          s"ScalusPhase: Expected an integer literal, but found identifier: ${value.show}",
                          value.srcPos
                        )
                        None
                    }
                case _ =>
                    report.warning(
                      s"ScalusPhase: Expected an integer literal, but found: ${value.show}",
                      posTree.srcPos.startPos
                    )
                    None // default value if parsing fails
            }
        }

        // Parse the arguments of the compiler options
        //  Note, that default values should be synchronized with the default value in scalus.Compiler.Options class
        //  (which is unaccessibke from here, so we should keep them in sync manually)
        def parseArg(arg: Tree, idx: Int, vals: List[Tree]): Unit = {
            arg match
                case tpd.NamedArg(name, value) =>
                    if name.toString == "targetLoweringBackend" then
                        parseTargetLoweringBackend(value, vals)
                    else if name.toString == "generateErrorTraces" then
                        generateErrorTraces = parseBooleanValue("generateErrorTraces", value, vals)
                            .getOrElse(SIRDefaultOptions.generateErrorTraces)
                    else if name.toString == "optimizeUplc" then {
                        optimizeUplc = parseBooleanValue("optimizeUplc", value, vals).getOrElse(
                          SIRDefaultOptions.optimizeUplc
                        )
                    } else if name.toString == "debug" then
                        debug = parseBooleanValue("debug", value, vals).getOrElse(false)
                    else if name.toString == "debugLevel" then
                        codeDebugLevel = parseIntValue("debugLevel", value, vals).getOrElse(0)
                    // else if name.toString == "useUniversalDataConversion" then {
                    //    useUniversalDataConversion = depend from the used backedm
                    //    useUniversalDataConversion = parseBooleanValue(value).getOrElse(true)
                    else if name.toString == "runtimeLinker" then
                        runtimeLinker = parseBooleanValue("runtimeLinker", value, vals).getOrElse(
                          SIRDefaultOptions.runtimeLinker
                        )
                    else if name.toString == "writeSIRToFile" then
                        writeSIRToFile = parseBooleanValue("writeSIRToFile", value, vals).getOrElse(
                          SIRDefaultOptions.writeSIRToFile
                        )
                    else {
                        report.warning(
                          s"ScalusPhase: Unknown compiler option: $name",
                          posTree.srcPos.startPos
                        )
                    }
                case value =>
                    idx match
                        case 0 => // targetLoweringBackend
                            parseTargetLoweringBackend(value, vals)
                        case 1 => // generateErrorTraces
                            generateErrorTraces =
                                parseBooleanValue("generateErrorTraces", value, vals).getOrElse(
                                  SIRDefaultOptions.generateErrorTraces
                                )
                        case 2 => // optimizeUplc
                            optimizeUplc = parseBooleanValue("optimizeUplc", value, vals).getOrElse(
                              SIRDefaultOptions.optimizeUplc
                            )
                        case 3 => // runtimeLinker
                            runtimeLinker =
                                parseBooleanValue("runtimeLinker", value, vals).getOrElse(
                                  SIRDefaultOptions.runtimeLinker
                                )
                        case 4 => // writeSIRToFile
                            writeSIRToFile =
                                parseBooleanValue("writeSIRToFile", value, vals).getOrElse(
                                  SIRDefaultOptions.writeSIRToFile
                                )
                        case 5 => // debugLevel
                            codeDebugLevel = parseIntValue("debugLevel", value, vals).getOrElse(0)
                        case 6 => // debug
                            debug =
                                parseBooleanValue("debug", value, vals).getOrElse(isCompilerDebug)
                            if debug && codeDebugLevel == 0 then codeDebugLevel = 10
                        case _ =>
                            report.warning(
                              s"ScalusPhase: too many position argiments for scalus.compiler.Options, expected max 4, but found ${idx + 1}",
                              posTree.srcPos.startPos
                            )
        }

        def parseCompilerOptionsApply(app: tpd.Apply, stats: List[Tree]): Unit = {
            // println("compiler options apply: " + app.show)
            if app.fun.symbol != Symbols.requiredMethod(
                  "scalus.Compiler.Options.apply"
                )
            then
                report.warning(
                  "expected scalus.Compiler.Options.apply",
                  posTree.srcPos
                )
            app.args.zipWithIndex.foreach { case (arg, idx) =>
                parseArg(arg, idx, stats)
            }
        }

        def findValdefWithName(name: Name, vals: List[Tree]): Option[ValDef] = {
            vals.collectFirst {
                case vd: ValDef if vd.symbol.name == name => vd
            }
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
                        case app @ tpd.Apply(obj, args) =>
                            // report.echo(s"defDefTree.rhs.apply, args=${args}")
                            parseCompilerOptionsApply(app, Nil)
                        case Block(stats, app @ tpd.Apply(obj, args)) =>
                            // report.echo(s"defDefTree.rhs.block.apply, args=${args}")
                            parseCompilerOptionsApply(app, stats)
                        case _ =>
                            report.warning(
                              s"ScalusPhase: Expected a call to scalus.Compiler.Options.apply, but found: ${underTyped.show}" +
                                  s"ntree:${underTyped}",
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
          backend = backend,
          debugLevel = if debugLevel == 0 then codeDebugLevel else debugLevel
        )
        if isCompilerDebug then
            println(
              s"retrieveCompilerOptions, retval=${retval}, useUniversalDataConversion=${useUniversalDataConversion}"
            )
        retval
    }

    def createLinkerOptionsTree(
        options: SIRCompilerOptions,
        pos: SrcPos
    )(using Context): tpd.Tree = {
        import tpd.*
        val linkerOptionsModule = requiredModule("scalus.sir.linking.SIRLinkerOptions")
        val linkerOptionsTree = ref(linkerOptionsModule).select(
          linkerOptionsModule.requiredMethod("apply")
        )
        val args = List(
          Literal(Constant(options.universalDataRepresentation)), // universalDataRepresentation
          Literal(Constant(true)), // print errors
          Literal(Constant(options.debugLevel)), // debug level
        )
        Apply(linkerOptionsTree, args).withSpan(pos.span)
    }

}
