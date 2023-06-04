package scalus

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.plugins.*

import scala.collection.immutable
import scala.language.implicitConversions

enum Mode:
  case Compile, Link

class Plugin extends StandardPlugin {
  val name: String = "scalus"
  override val description: String = "Compile Scala to Scalus IR"

  def init(options: List[String]): List[PluginPhase] =
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
      val arg = tree.args.head
      val compiler = new SIRCompiler(Mode.Link)
      val result = arg match
        case Block(
              List(DefDef(_, _, _, Apply(code, _))),
              Closure(Nil, Ident(_), EmptyTree)
            ) =>
          // report.echo(s"compile: ${code.show}")
          compiler.compileToSIR(code.asInstanceOf[Tree])
        case code =>
          // report.echo(s"compile: ${arg.show}")
          compiler.compileToSIR(code)
      val converter = new SIRConverter
      converter.convert(result)
    else tree
  end transformApply
}
