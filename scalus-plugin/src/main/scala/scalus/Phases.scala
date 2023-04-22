package scalus

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.{Context, *}
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.SymDenotations.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.plugins.*

import scala.language.implicitConversions

//case class DataDecl(name: String, params: List[String], constructors: List[DataCtor])
//case class DataCtor(name: String, params: List[Type])

class PhaseA extends PluginPhase {
  import tpd.*

  val phaseName = "PhaseA"

  private var enterSym: Symbol = _

  override val runsAfter = Set(transform.Pickler.name)
//  override val runsAfter = Set("elimByName")
  override val runsBefore = Set("patternMatcher")

  override def prepareForUnit(tree: Tree)(using Context): Context =
    report.echo(s"PhaseA: ${ctx.compilationUnit.source.file.name}")
    report.echo(tree.showIndented(2))
    report.echo(tree.toString)

    ctx

  override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree =
    val compileSymbol = requiredModule("scalus.uplc.Compiler").requiredMethod("compile")
    report.echo(s"PhaseA: ${tree.fun.symbol.name}")
    if tree.fun.symbol == compileSymbol then
      report.echo(tree.showIndented(2))
      val arg = tree.args.head
      val result = arg match
        case Block(
              List(DefDef(_, _, _, Apply(code, _))),
              Closure(Nil, Ident(_), EmptyTree)
            ) =>
          report.echo(s"compile: ${code.show}")
          transpile(code.asInstanceOf[Tree]) // FIXME instanceof
        case code =>
          report.echo(s"compile: ${arg.show}")
          transpile(code)
      result
    else tree
  end transformApply

  /*
  private def transpileConst(const: Constant)(using Context): Tree = {
    import sir.SIR.*
    const.tag match
      case Constants.BooleanTag => Const(uplc.Constant.Bool(const.booleanValue))
      case Constants.StringTag  => Const(uplc.Constant.String(const.stringValue))
      case Constants.UnitTag    => Const(uplc.Constant.Unit)
      case _                    => report.error(s"Unsupported constant type $const"); EmptyTree
  }
   */
  private def transpile(tree: Tree)(using Context): Tree =
    val errSymbol = requiredModule("scalus.sir.SIR.Error").requiredMethod("apply")
    val app = ref(errSymbol).appliedTo(Literal(Constant("invalid")))

//    val result = tree match
//      case Literal(const) => transpileConst(const)
//      case _ => report.error(s"Unsupported expression: ${tree.show}")

    app

}
