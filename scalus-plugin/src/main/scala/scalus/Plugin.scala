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
        val compiler = new SIRCompiler(Mode.Compile)
        compiler.compileModule(tree)
        ctx

    override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree =
        val compileSymbol = requiredModule("scalus.Compiler").requiredMethod("compile")
        if tree.fun.symbol == compileSymbol then
            // report.echo(tree.showIndented(2))
            val code = tree.args.head
            val compiler = new SIRCompiler(Mode.Link)
            val result = compiler.compileToSIR(code)
            val converter = new SIRConverter
            converter.convertSIRToTree(result)
        else tree
    end transformApply
}
