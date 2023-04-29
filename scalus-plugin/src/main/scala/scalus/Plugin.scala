package scalus

import dotty.tools.dotc.plugins.*
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
import scala.collection.{immutable, mutable}
import scalus.sir.SIR
import scalus.uplc
import java.io.FileOutputStream
import java.io.BufferedOutputStream
import scalus.sir.DataDecl
import scala.annotation.threadUnsafe
import dotty.tools.dotc.core.Types.TypeRef
import scalus.uplc.NamedDeBruijn
import scalus.uplc.DefaultFun

class Plugin extends StandardPlugin {
  val name: String = "scalus"
  override val description: String = "Compile Scala to Scalus IR"

  def init(options: List[String]): List[PluginPhase] =
    new ScalusPhase :: Nil
}



//case class DataDecl(name: String, params: List[String], constructors: List[DataCtor])
//case class DataCtor(name: String, params: List[Type])

class ScalusPhase extends PluginPhase {
  import tpd.*

  val phaseName = "Scalus"

  private var enterSym: Symbol = _

  // override val runsAfter = Set("initChecker")
  override val runsAfter = Set("firstTransform")
  override val runsBefore = Set("patternMatcher")

  /* private def genIRFile(cunit: CompilationUnit, tree: ir.Trees.ClassDef): Unit = {
    val outfile = getFileFor(cunit, tree.name.name, ".sjsir")
    val output = outfile.bufferedOutput
    try {
      ir.Serializers.serialize(output, tree)
    } finally {
      output.close()
    }
  }

  private def getFileFor(cunit: CompilationUnit, className: ClassName,
      suffix: String): dotty.tools.io.AbstractFile = {
    val outputDirectory = ctx.settings.outputDir.value
    val pathParts = className.nameString.split('.')
    val dir = pathParts.init.foldLeft(outputDirectory)(_.subdirectoryNamed(_))
    val filename = pathParts.last
    dir.fileNamed(filename + suffix)
   }*/

  type Env = immutable.HashSet[Symbol]

  /* case class B(name: String, symbol: Symbol, recursivity: sir.Recursivity, body: SIR):
    def fullName = symbol.name

  enum CompileDef:
    case Compiling
    case Compiled(binding: B)

  val globalDefs: mutable.LinkedHashMap[Symbol, CompileDef] = mutable.LinkedHashMap.empty
  val globalDataDecls: mutable.LinkedHashMap[Symbol, DataDecl] = mutable.LinkedHashMap.empty */

  override def prepareForUnit(tree: Tree)(using Context): Context =
    report.echo(s"Scalus: ${ctx.compilationUnit.source.file.name}")
    // report.echo(tree.showIndented(2))
    // report.echo(tree.toString)

    val sir = compileToSIR(tree)
    val suffix = ".sir"
    val outputDirectory = ctx.settings.outputDir.value
    val filename = outputDirectory.fileNamed(ctx.compilationUnit.source.file.name + ".sir")
    val output = filename.bufferedOutput
    val oos = new java.io.ObjectOutputStream(output)
    oos.writeObject(sir)
    oos.close()
    output.close()

    ctx

  override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree =
    val compileSymbol = requiredModule("scalus.uplc.Compiler").requiredMethod("compile")
    // report.echo(s"PhaseA: ${tree.fun.symbol.name}")
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
      val converter = new SIRConverter
      converter.convert(result)
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

  def compileToSIR(tree: Tree)(using ctx: Context): SIR = {
    import sir.SIR.*

    def collectTypeDefs(tree: Tree): List[TypeDef] = {
      @threadUnsafe lazy val CompileAnnotType: TypeRef = requiredClassRef("scalus.Compile")
      def CompileAnnot(using Context) = CompileAnnotType.symbol.asClass

      tree match {
        case EmptyTree            => Nil
        case PackageDef(_, stats) => stats.flatMap(collectTypeDefs)
        case cd: TypeDef =>
          if cd.symbol.hasAnnotation(CompileAnnot) then List(cd)
          else Nil
        case _: ValDef    => Nil // module instance
        case Import(_, _) => Nil

      }
    }

    case class AdtTypeInfo(
        constructorTypeSymbol: Symbol,
        dataTypeSymbol: Symbol,
        constructors: List[Symbol]
    )

    def getAdtInfoFromConstroctorType(constrTpe: Type): AdtTypeInfo = {
      /* We support these cases:
        1. case class Foo(a: Int, b: String)
        2. case object Bar
        3. enum Base { case A ...}
        4. enum Base { case B(a, b) }
        5. sealed abstract class Base; object Base { case object A extends Base }
        6. sealed abstract class Base; object Base { case class B(a: Int, b: String) extends Base }

       */
      val typeSymbol = constrTpe.dealias.widen.typeSymbol
      // look for a base `sealed abstract class`. If it exists, we are in case 5 or 6
      val adtBaseType = constrTpe.baseClasses.find(b =>
        b.flags.isOneOf(Flags.Sealed | Flags.Abstract) && !b.flags.is(Flags.Trait)
      )

      val info =
        adtBaseType match
          case None => // case 1 or 2
            AdtTypeInfo(typeSymbol, typeSymbol, List(typeSymbol))
          case Some(baseClassSymbol) if constrTpe.isSingleton => // case 3, 5
            AdtTypeInfo(constrTpe.termSymbol, baseClassSymbol, baseClassSymbol.children)
          case Some(baseClassSymbol) => // case 4, 6
            AdtTypeInfo(typeSymbol, baseClassSymbol, baseClassSymbol.children)
      report.debuglog(s"adtBaseType: ${constrTpe.show} ${typeSymbol} ${adtBaseType} $info")
      info
    }

    def primaryConstructorParams(typeSymbol: Symbol): List[Symbol] = {
      val fields = typeSymbol.primaryConstructor.paramSymss.flatten.filter(s => s.isTerm)
      // debugInfo(s"caseFields: ${typeSymbol.fullName} $fields")
      fields
    }

    def compileTypeDef(td: TypeDef) = {
      println(s"TypeDef: ${td.name}: ${td.symbol.annotations
          .map(_.symbol.fullName)}, case class: ${td.tpe.typeSymbol.is(Flags.CaseClass)}")
      if td.tpe.typeSymbol.is(Flags.CaseClass) then compileCaseClass(td)
      else
        val tpl = td.rhs.asInstanceOf[Template]
        tpl.body.foreach {
          case dd: DefDef =>
            println(s"DefDef: ${dd.name}")
          case _ =>
        }
    }

    def compileCaseClass(td: TypeDef) = {
      println(s"compileCaseClass: ${td.name}")

    }

    val allTypeDefs = collectTypeDefs(tree)
    println(allTypeDefs.map(td => s"${td.name} ${td.isClassDef}"))

    allTypeDefs.foreach(compileTypeDef)

    /* tree match
      case Literal(const) =>
        const.tag match
          case Constants.BooleanTag => Const(uplc.Constant.Bool(const.booleanValue))
          case Constants.StringTag  => Const(uplc.Constant.String(const.stringValue))
          case Constants.UnitTag    => Const(uplc.Constant.Unit)
          case _ =>
            report.error(s"Unsupported constant type $const"); Error("Unsupported constant type")
      case _ =>
        report.error(s"Unsupported expression: ${tree.show}") */
    Const(uplc.Constant.Bool(false))
  }

  private def transpile(tree: Tree)(using Context): SIR = tree match
    case Literal(const) =>
      const.tag match
        case Constants.BooleanTag => SIR.Const(uplc.Constant.Bool(const.booleanValue))
        case Constants.StringTag  => SIR.Const(uplc.Constant.String(const.stringValue))
        case Constants.UnitTag    => SIR.Const(uplc.Constant.Unit)
        case _ =>
          report.error(s"Unsupported constant type $const");
          SIR.Error("Unsupported constant type")

}

class SIRConverter(using Context) {
  import tpd.*
  /*
  enum SIR:
  case Var(name: NamedDeBruijn) extends SIR
  case Let(recursivity: Recursivity, bindings: List[Binding], body: SIR) extends SIR
  case LamAbs(name: String, term: SIR) extends SIR
  case Apply(f: SIR, arg: SIR) extends SIR
  case Const(const: Constant) extends SIR
  case IfThenElse(cond: SIR, t: SIR, f: SIR) extends SIR
  case Builtin(bn: DefaultFun) extends SIR
  case Error(msg: String) extends SIR
  case Decl(data: DataDecl, term: SIR) extends SIR
  case Constr(name: String, data: DataDecl, args: List[SIR]) extends SIR
  case Match(scrutinee: SIR, cases: List[Case]) extends SIR
   */
  val ErrorSymbol = requiredModule("scalus.sir.SIR.Error")
  val ConstSymbol = requiredModule("scalus.sir.SIR.Const")
  val ApplySymbol = requiredModule("scalus.sir.SIR.Apply")
  val ConstantIntegerSymbol = requiredModule("scalus.uplc.Constant.Integer")
  val ConstantBoolSymbol = requiredModule("scalus.uplc.Constant.Bool")
  val VarSymbol = requiredModule("scalus.sir.SIR.Var")
  val LetSymbol = requiredModule("scalus.sir.SIR.Let")
  val LamAbsSymbol = requiredModule("scalus.sir.SIR.LamAbs")
  val NamedDeBruijnSymbol = requiredModule("scalus.uplc.NamedDeBruijn")
  val BuiltinSymbol = requiredModule("scalus.sir.SIR.Builtin")
  def mkApply(f: SIR, arg: SIR) =
    ref(ApplySymbol.requiredMethod("apply")).appliedToArgs(List(convert(f), convert(arg)))
  def mkLamAbs(name: String, term: SIR) =
    ref(LamAbsSymbol.requiredMethod("apply"))
      .appliedToArgs(List(Literal(Constant(name)), convert(term)))
  def mkVar(name: NamedDeBruijn) =
    ref(VarSymbol.requiredMethod("apply")).appliedTo(convert(name))
  def mkConst(const: scalus.uplc.Constant) =
    ref(ConstSymbol.requiredMethod("apply")).appliedTo(convert(const))
  def mkNamedDeBruijn(name: String) =
    ref(NamedDeBruijnSymbol.requiredMethod("apply")).appliedTo(Literal(Constant(name)))
  def mkBuiltin(bn: DefaultFun) =
    ref(BuiltinSymbol.requiredMethod("apply")).appliedTo(convert(bn))
  def mkDefaultFun(fun: DefaultFun) = ref(requiredModule(fun.toString()))

  def mkError(msg: String) =
    ref(ErrorSymbol.requiredMethod("apply")).appliedTo(Literal(Constant(msg)))

  def convert(fun: DefaultFun): Tree = mkDefaultFun(fun)
  def convert(name: NamedDeBruijn): Tree = mkNamedDeBruijn(name.name)
  def convert(value: Boolean): Tree = Literal(Constant(value))
  def convert(const: scalus.uplc.Constant): Tree = {
    const match
      case uplc.Constant.Bool(value) =>
        ref(ConstantBoolSymbol.requiredMethod("apply")).appliedTo(convert(value))
  }

  def convert(sir: SIR): Tree = {
    import SIR.*
    sir match
      case Error(msg)         => mkError(msg)
      case Var(name)          => mkVar(name)
      case Const(const)       => mkConst(const)
      case Apply(f, arg)      => mkApply(f, arg)
      case LamAbs(name, term) => mkLamAbs(name, term)
      case Builtin(bn)        => mkBuiltin(bn)
      case _ =>
        report.error(s"Unsupported expression: ${sir}")
        mkError("Unsupported expression")
  }
}
