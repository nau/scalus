package scalus

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.SymDenotations.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Types.TypeRef
import dotty.tools.dotc.core.*
import dotty.tools.dotc.plugins.*
import dotty.tools.dotc.plugins.*
import scalus.sir.Binding
import scalus.sir.Case
import scalus.sir.ConstrDecl
import scalus.sir.DataDecl
import scalus.sir.Recursivity
import scalus.sir.SIR
import scalus.uplc
import scalus.uplc.DefaultFun
import scalus.uplc.NamedDeBruijn

import java.io.BufferedOutputStream
import java.io.FileOutputStream
import scala.annotation.threadUnsafe
import scala.collection.immutable
import scala.collection.mutable
import scala.language.implicitConversions
import scalus.builtins.ByteString
import scalus.uplc.Constant.Data
import scalus.uplc.DefaultUni
import scala.util.control.NonFatal

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

    val compiler = new SIRCompiler
    val sir = compiler.compileToSIR(tree)
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
      val compiler = new SIRCompiler
      val result = arg match
        case Block(
              List(DefDef(_, _, _, Apply(code, _))),
              Closure(Nil, Ident(_), EmptyTree)
            ) =>
          report.echo(s"compile: ${code.show}")
          compiler.transpile(code.asInstanceOf[Tree]) // FIXME instanceof
        case code =>
          report.echo(s"compile: ${arg.show}")
          compiler.transpile(code)
      val converter = new SIRConverter
      converter.convert(result)
    else tree
  end transformApply
}

class SIRCompiler(using ctx: Context) {
  import tpd.*
  type Env = immutable.HashSet[Symbol]

  val converter = new SIRConverter

  def compileToSIR(tree: Tree): SIR = {
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

  def compileConstant: PartialFunction[Tree, scalus.uplc.Constant] = {
    case Literal(c: Constant) =>
      c.tag match
        case Constants.BooleanTag => scalus.uplc.Constant.Bool(c.booleanValue)
        case Constants.StringTag  => scalus.uplc.Constant.String(c.stringValue)
        case Constants.UnitTag    => scalus.uplc.Constant.Unit
        case Constants.IntTag =>
          report.error(s"Scalus: Int literals are not supported. Try BigInt(${c.intValue}) instead")
          scalus.uplc.Constant.Unit
        case _ =>
          report.error(s"Unsupported constant type $c");
          scalus.uplc.Constant.Unit

    case e @ Literal(_) =>
      report.error(s"compileExpr: Unsupported literal ${e.show}\n$e")
      scalus.uplc.Constant.Unit
    case Apply(bigintApply, List(Literal(c)))
        if bigintApply.symbol.showFullName == "scala.math.BigInt.apply" =>
      c.tag match
        case Constants.IntTag =>
          scalus.uplc.Constant.Integer(BigInt(c.intValue))
        case Constants.StringTag =>
          scalus.uplc.Constant.Integer(BigInt(c.stringValue))
        case _ =>
          report.error(s"Unsupported constant type $c");
          scalus.uplc.Constant.Unit
    case Apply(i, List(Literal(c)))
        if i.symbol.showFullName == "scala.math.BigInt.int2bigInt" =>
      scalus.uplc.Constant.Integer(BigInt(c.intValue))
    case expr if expr.symbol.showFullName == "scalus.builtins.ByteString.empty" =>
      scalus.uplc.Constant.ByteString(scalus.builtins.ByteString.empty)
    case Apply(expr, List(Literal(c)))
        if expr.symbol.showFullName == "scalus.builtins.ByteString.fromHex" =>
      scalus.uplc.Constant.ByteString(scalus.builtins.ByteString(hexToBytes(c.stringValue)))

  }

  def hexToBytes(hex: String): Array[Byte] =
    val hexString = hex.replace(" ", "")
    try
      if (hexString.length & 1) != 0 then sys.error("string length is not even")
      hexString.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
    catch
      case NonFatal(e) =>
        throw new IllegalArgumentException(s"`$hexString` is not a valid hex string", e)

  def compileExpr(env: immutable.HashSet[Symbol], tree: Tree)(using Context): SIR = {
    if compileConstant.isDefinedAt(tree) then
      val const = compileConstant(tree)
      SIR.Const(const)
    else
      tree match
        case If(cond, t, f) =>
          SIR.IfThenElse(transpile(cond), transpile(t), transpile(f))
        // throw new Exception("error msg")
        // Supports any exception type that uses first argument as message
        case Apply(Ident(nme.throw_), immutable.List(ex)) =>
          val msg = ex match
            case Apply(
                  Select(New(tpt), nme.CONSTRUCTOR),
                  immutable.List(Literal(msg), _*)
                ) if tpt.tpe <:< defn.ExceptionClass.typeRef =>
              msg.stringValue
            case term => "error"
          SIR.Error(msg)
        case Typed(expr, _) => compileExpr(env, expr)
        case x =>
          report.error(s"Unsupported expression: ${x.show}\n$x")
          SIR.Error("Unsupported expression")
  }

  def transpile(tree: Tree)(using Context): SIR = {
    val result = compileExpr(immutable.HashSet.empty, tree)
    result
  }

}

