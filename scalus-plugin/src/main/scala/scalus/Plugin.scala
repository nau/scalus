package scalus

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.plugins.*

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

class ScalusPhase extends PluginPhase {
    import tpd.*

    val phaseName = "Scalus"

    // override val runsAfter = Set("initChecker")
    override val runsAfter = Set("firstTransform")
    override val runsBefore = Set("patternMatcher")

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

    override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree =
        val compilerModule = requiredModule("scalus.Compiler")
        val compileSymbol = compilerModule.requiredMethod("compile")
        val compileDebugSymbol = compilerModule.requiredMethod("compileDebug")
        if tree.fun.symbol == compileSymbol || tree.fun.symbol == compileDebugSymbol then
            println(
              s"Scalus: Found compile call:  ${tree.show}, code=${tree.args.head.show}, debug=${tree.fun.symbol == compileDebugSymbol}"
            )
            // report.echo(tree.showIndented(2))
            val code = tree.args.head
            val compiler = new SIRCompiler(Mode.Link)
            val result = compiler.compileToSIR(code, tree.fun.symbol == compileDebugSymbol)
            val converter = new SIRConverter
            converter.convertSIRToTree(result, tree.span)
        else tree
    end transformApply
}
