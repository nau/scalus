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
import scala.collection.mutable.ListBuffer
import java.net.URL
import dotty.tools.io.ClassPath

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
    compiler.compileToSIR(tree)
    ctx

  override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree =
    val compileSymbol = requiredModule("scalus.uplc.Compiler").requiredMethod("compile")
    // report.echo(s"PhaseA: ${tree.fun.symbol.name}")
    if tree.fun.symbol == compileSymbol then
      // report.echo(tree.showIndented(2))
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

case class B(name: String, symbol: Symbol, recursivity: Recursivity, body: SIR):
  def fullName(using Context) = symbol.showFullName

class SIRCompiler(using ctx: Context) {
  import tpd.*
  type Env = immutable.HashSet[Symbol]

  val converter = new SIRConverter

  enum CompileDef:
    case Compiling
    case Compiled(binding: B)

  val globalDefs: mutable.LinkedHashMap[Symbol, CompileDef] = mutable.LinkedHashMap.empty

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
      println(
        s"TypeDef: ${td.name}: ${td.symbol.annotations
            .map(_.symbol.fullName)}, case class: ${td.tpe.typeSymbol.is(Flags.CaseClass)}, ${td.symbol.fullName}"
      )
      if td.tpe.typeSymbol.is(Flags.CaseClass) then compileCaseClass(td)
      else
        val tpl = td.rhs.asInstanceOf[Template]
        val bindings = tpl.body.collect {
          case dd: DefDef if !dd.symbol.flags.is(Flags.Synthetic) =>
            compileStmt(immutable.HashSet.empty, dd)
        }
        val sir = bindings.foldRight(SIR.Const(uplc.Constant.Unit)) { (bind, expr) =>
          SIR.Let(bind.recursivity, List(Binding(bind.name, bind.body)), expr)
        }

        val suffix = ".sir"
        val outputDirectory = ctx.settings.outputDir.value
        val className = td.symbol.fullName.show
        val pathParts = className.split('.')
        val dir = pathParts.init.foldLeft(outputDirectory)(_.subdirectoryNamed(_))
        val filename = pathParts.last
        val output = dir.fileNamed(filename + suffix).bufferedOutput
        val oos = new java.io.ObjectOutputStream(output)
        oos.writeObject(sir)
        oos.close()
        output.close()
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

  def findAndReadSIRFileOfSymbol(symbol: Symbol): Option[SIR] = {

    def getResources(packageName: String): Seq[URL] = {
      import scala.collection.JavaConverters._
      val packagePath = packageName.replace('.', '/')
      val classLoader = Thread.currentThread().getContextClassLoader
      val resources: java.util.Enumeration[URL] = classLoader.getResources(packagePath)
      resources.asScala.toList
    }

    def makeMacroClassLoader(using Context): ClassLoader = {
      import scala.language.unsafeNulls

      val entries = ClassPath.expandPath(ctx.settings.classpath.value, expandStar = true)
      val urls = entries.map(cp => java.nio.file.Paths.get(cp).toUri.toURL).toArray
      val out = Option(
        ctx.settings.outputDir.value.toURL
      ) // to find classes in case of suspended compilation
      new java.net.URLClassLoader(urls ++ out.toList, getClass.getClassLoader)
    }

    val filename = symbol.owner.fullName.show.replace('.', '/') + ".sir"
    println(
      s"findAndReadSIRFileOfSymbol: ${symbol.isClass}, ${filename}, ${getResources("scalus")}, ${ctx.settings.classpath.value}"
    )
    // read the file from the classpath
    val resource = makeMacroClassLoader.getResourceAsStream(filename)
    if resource != null then
      val ois = new java.io.ObjectInputStream(resource)
      val sir = ois.readObject().asInstanceOf[SIR]
      ois.close()
      Some(sir)
    else None
  }

  def compileIdentOrQualifiedSelect(env: Env, e: Tree): SIR = {
    println(s"Ident: ${e.symbol}, flags: ${e.symbol.flags.tryToShow}, term: ${e.show}")
    val isInLocalEnv = env.contains(e.symbol)
    val isInGlobalEnv = globalDefs.contains(e.symbol)
    (isInLocalEnv, isInGlobalEnv) match
      // global def, self reference, use the name
      case (true, true) => SIR.Var(NamedDeBruijn(e.symbol.fullName.show))
      // local def, use the name
      case (true, false) => SIR.Var(NamedDeBruijn(e.symbol.name.show))
      // global def, use full name
      case (false, true) => SIR.Var(NamedDeBruijn(e.symbol.fullName.show))
      case (false, false) =>
        if e.symbol.defTree == EmptyTree then
          findAndReadSIRFileOfSymbol(e.symbol) match
            case Some(sir) => sir
            case None =>
              report.error(s"Symbol ${e.symbol} is not defined")
              return SIR.Error("Symbol not defined")
        else
          // remember the symbol to avoid infinite recursion
          globalDefs.update(e.symbol, CompileDef.Compiling)
          // println(s"Tree of ${e}: ${e.tpe} isList: ${e.isList}")
          // debugInfo(s"Tree of ${e.symbol}: ${e.symbol.tree.show}\n${e.symbol.tree}")
          val b = compileStmt(immutable.HashSet.empty, e.symbol.defTree)
          // remove the symbol from the linked hash map so the order of the definitions is preserved
          globalDefs.remove(e.symbol)
          globalDefs.update(e.symbol, CompileDef.Compiled(b))
          SIR.Var(NamedDeBruijn(e.symbol.fullName.show))
  }

  def compileStmt(env: Env, stmt: Tree): B = {
    // debugInfo(s"compileStmt  ${stmt.show} in ${env}")
    stmt match
      case vd @ ValDef(name, _, _) =>
        val bodyExpr = compileExpr(env, vd.rhs)
        B(name.show, vd.symbol, Recursivity.NonRec, bodyExpr)
      case dd @ DefDef(name, paramss, tpe, _) =>
        val params = paramss.flatten.collect({ case vd: ValDef => vd })
        val body = dd.rhs
        val bodyExpr: scalus.sir.SIR = {
          if params.isEmpty then
            val bE = compileExpr(env + stmt.symbol, body)
            SIR.LamAbs("_", bE)
          else
            val symbols = params.map { case v: ValDef => v.symbol }
            val bE = compileExpr(env ++ symbols + stmt.symbol, body)
            symbols.foldRight(bE) { (symbol, acc) =>
              SIR.LamAbs(symbol.name.show, acc)
            }
        }
        B(name.show, stmt.symbol, Recursivity.Rec, bodyExpr)
      /*
        case ValDef(name, _, _) =>
          report.errorAndAbort(
            s"""compileStmt: val ${stmt.symbol.fullName} has no body. Try adding "scalacOptions += "-Yretain-trees" to your build.sbt"""
          )
        case DefDef(name, args, tpe, None) =>
          report.errorAndAbort(
            s"""compileStmt: def ${stmt.symbol.fullName} has no body. Try adding "scalacOptions += "-Yretain-trees" to your build.sbt"""
          ) */
      case x =>
        B("_", NoSymbol, Recursivity.NonRec, compileExpr(env, x))

      // case x => report.error(s"compileStmt: $x", stmt.sourcePos)
  }

  def compileBlock(env: Env, stmts: immutable.List[Tree], expr: Tree): SIR = {
    val exprs = ListBuffer.empty[B]
    val exprEnv = stmts.foldLeft(env) { case (env, stmt) =>
      val bind = compileStmt(env, stmt)
      exprs += bind
      env + bind.symbol
    }
    val exprExpr = compileExpr(exprEnv, expr)
    exprs.foldRight(exprExpr) { (bind, expr) =>
      SIR.Let(bind.recursivity, List(Binding(bind.name, bind.body)), expr)
    }
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
    case Apply(i, List(Literal(c))) if i.symbol.showFullName == "scala.math.BigInt.int2bigInt" =>
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
        // TODO support Ident, when equalsInteger is imported
        case bi: Select if bi.symbol.showFullName == "scalus.builtins.Builtins.equalsInteger" =>
          SIR.Builtin(DefaultFun.EqualsInteger)
        // FIXME: This is wrong. Just temporary
        case tree @ Ident(a) =>
          compileIdentOrQualifiedSelect(env, tree)
        // f.apply(arg) => Apply(f, arg)
        case Apply(Select(f, nme.apply), args) if defn.isFunctionType(f.tpe.widen) =>
          val fE = compileExpr(env, f)
          val argsE = args.map(compileExpr(env, _))
          argsE.foldLeft(fE)((acc, arg) => SIR.Apply(acc, arg))
        // Ignore type application
        case TypeApply(f, args) => compileExpr(env, f)
        // Generic Apply
        case Apply(f, args) =>
          val fE = compileExpr(env, f)
          val argsE = args.map(compileExpr(env, _))
          if argsE.isEmpty then SIR.Apply(fE, SIR.Const(scalus.uplc.Constant.Unit))
          else argsE.foldLeft(fE)((acc, arg) => SIR.Apply(acc, arg))
        // (x: T) => body
        case Block(
              immutable.List(
                dd @ DefDef(nme.ANON_FUN, _, _, _)
              ),
              Closure(_, Ident(nme.ANON_FUN), _)
            ) =>
          compileStmt(env, dd).body
        case Block(stmt, expr) => compileBlock(env, stmt, expr)
        case Typed(expr, _)    => compileExpr(env, expr)
        case x =>
          report.error(s"Unsupported expression: ${x.show}\n$x")
          SIR.Error("Unsupported expression")
  }

  def transpile(tree: Tree)(using Context): SIR = {
    println(s"transpile: ${tree}")
    val result = compileExpr(immutable.HashSet.empty, tree)
    result
  }

}
