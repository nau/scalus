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
import dotty.tools.dotc.plugins.*

import scala.language.implicitConversions

//case class DataDecl(name: String, params: List[String], constructors: List[DataCtor])
//case class DataCtor(name: String, params: List[Type])

sealed trait SIR
case class Let(name: String, value: SIR, body: SIR) extends SIR
case class If(cond: SIR, t: SIR, f: SIR) extends SIR
case class Term(uplc: scalus.uplc.Term) extends SIR

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

  override def transformValDef(tree: tpd.ValDef)(using Context): tpd.Tree =
    report.echo(s"ValDef: ${tree}")
    report.echo(tree.showIndented(2))
    val errSymbol = requiredModule("scalus.uplc.Term.Error").requiredMethod("apply")
    val app = ref(errSymbol).appliedTo(Literal(Constant("invalid")))

//    val converted = Typed(app,TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class uplc)),class Term)])
    tpd.cpy.ValDef(tree)(rhs = app)
}
