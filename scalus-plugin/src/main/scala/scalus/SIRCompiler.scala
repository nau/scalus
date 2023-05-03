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
import scalus.flat.Flat.Flat
import scalus.flat.Flat.EncoderState
import scalus.flat.Flat
import scalus.flat.FlatInstantces.given
import scalus.flat.Flat.DecoderState

enum Module:
  case DataDecl(decl: scalus.sir.DataDecl)
  case Module(defs: List[Binding])

case class B(name: String, symbol: Symbol, recursivity: Recursivity, body: SIR):
  def fullName(using Context) = symbol.showFullName

class SIRCompiler(mode: scalus.Mode)(using ctx: Context) {
  import tpd.*
  type Env = immutable.HashSet[Symbol]

  val converter = new SIRConverter

  extension (t: Type)
    def isPair: Boolean =
      // FIXME: this is a hack, should be something like List above, but it doesn't work for some reason
      val r = t.typeSymbol.showFullName == "scalus.builtins.Pair"
      println(s"$t is pair: $r, ${t.typeSymbol.showFullName}")
      r
    def isList: Boolean =
      t <:< requiredClass("scalus.builtins.List").typeRef.appliedTo(defn.AnyType)

  extension (t: Tree) def isList: Boolean = t.tpe.isList

  extension (t: Tree) def isPair: Boolean = t.tpe.isPair

  extension (t: Tree) def isLiteral = compileConstant.isDefinedAt(t)
  extension (t: Tree) def isData = t.tpe <:< requiredClass("scalus.uplc.Data").typeRef

  enum CompileDef:
    case Compiling
    case Compiled(binding: B)

  val globalDefs: mutable.LinkedHashMap[Symbol, CompileDef] = mutable.LinkedHashMap.empty
  val globalDataDecls: mutable.LinkedHashMap[Symbol, DataDecl] = mutable.LinkedHashMap.empty
  val moduleCache: mutable.LinkedHashMap[Symbol, Module] = mutable.LinkedHashMap.empty

  def compileModule(tree: Tree): Unit = {
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
      report.echo(s"adtBaseType: ${constrTpe.show} ${typeSymbol} ${adtBaseType} $info")
      info
    }

    def primaryConstructorParams(typeSymbol: Symbol): List[Symbol] = {
      val fields = typeSymbol.primaryConstructor.paramSymss.flatten.filter(s => s.isTerm)
      // debugInfo(s"caseFields: ${typeSymbol.fullName} $fields")
      fields
    }

    val allTypeDefs = collectTypeDefs(tree)
    println(allTypeDefs.map(td => s"${td.name} ${td.isClassDef}"))

    allTypeDefs.foreach(compileTypeDef)
  }

  def compileTypeDef(td: TypeDef) = {
    report.echo(
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
      val module = Module.Module(bindings.map(b => Binding(b.name, b.body)))
      val suffix = ".sir"
      val outputDirectory = ctx.settings.outputDir.value
      val className = td.symbol.fullName.show
      val pathParts = className.split('.')
      val dir = pathParts.init.foldLeft(outputDirectory)(_.subdirectoryNamed(_))
      val filename = pathParts.last
      val output = dir.fileNamed(filename + suffix).bufferedOutput
      val fl = summon[Flat[Module]]
      val enc = EncoderState(fl.bitSize(module) / 8 + 1)
      Flat.encode(module, enc)
      enc.filler()
      output.write(enc.buffer)
      output.close()
  }

  def compileCaseClass(td: TypeDef) = {
    println(s"compileCaseClass: ${td.name}")

  }

  def findAndReadModuleOfSymbol(symbol: Symbol): Option[Module] = {

    def getResources(packageName: String): Seq[URL] = {
      import scala.collection.JavaConverters._
      val packagePath = packageName.replace('.', '/')
      val classLoader = Thread.currentThread().getContextClassLoader
      val resources: java.util.Enumeration[URL] = classLoader.getResources(packagePath)
      resources.asScala.toList
    }

    def makeClassLoader(using Context): ClassLoader = {
      import scala.language.unsafeNulls

      val entries = ClassPath.expandPath(ctx.settings.classpath.value, expandStar = true)
      val urls = entries.map(cp => java.nio.file.Paths.get(cp).toUri.toURL).toArray
      val out = Option(
        ctx.settings.outputDir.value.toURL
      ) // to find classes in case of suspended compilation
      new java.net.URLClassLoader(urls ++ out.toList, getClass.getClassLoader)
    }

    val filename = symbol.owner.fullName.show.replace('.', '/') + ".sir"
    println(s"findAndReadModuleOfSymbol: ${symbol.isClass}, ${filename}")
    // read the file from the classpath
    val resource = makeClassLoader.getResourceAsStream(filename)
    if resource != null then
      val buffer = resource.readAllBytes()
      val dec = DecoderState(buffer)
      val module = Flat.decode[Module](dec)
      resource.close()
      Some(module)
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
      case (false, false) if mode == scalus.Mode.Compile =>
        SIR.Var(NamedDeBruijn(e.symbol.fullName.show))
      case (false, false) =>
        if e.symbol.defTree == EmptyTree then
          moduleCache.get(e.symbol.owner) match
            case Some(Module.Module(defs)) =>
              val binding = defs.find(b => b.name == e.symbol.name.show).get
              globalDefs.update(
                e.symbol,
                CompileDef.Compiled(B(binding.name, e.symbol, Recursivity.Rec, binding.value))
              )
              binding.value
            case _ =>
              findAndReadModuleOfSymbol(e.symbol) match
                case Some(m @ Module.Module(defs)) =>
                  moduleCache.put(e.symbol.owner, m)
                  defs.head.value
                case Some(Module.DataDecl(decl)) =>
                  report.error(s"Read DataDecl instead of ${e.symbol.fullName.show}: ${decl}")
                  return SIR.Error(s"Read DataDecl instead of ${e.symbol.fullName.show}: ${decl}")
                case None =>
                  report.error(s"Symbol ${e.symbol.fullName.show} is not defined")
                  return SIR.Error(s"Symbol ${e.symbol.fullName.show} not defined")
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

  object BuiltinHelper {
    def builtinFun(tpe: String): Option[SIR.Builtin] = {
      val DefaultFunValues = Map(
        "scalus.builtins.Builtins.mkConstr" -> DefaultFun.ConstrData,
        "scalus.builtins.Builtins.mkList" -> DefaultFun.ListData,
        "scalus.builtins.Builtins.mkMap" -> DefaultFun.MapData,
        "scalus.builtins.Builtins.mkB" -> DefaultFun.BData,
        "scalus.builtins.Builtins.mkI" -> DefaultFun.IData,
        "scalus.builtins.Builtins.unsafeDataAsConstr" -> DefaultFun.UnConstrData,
        "scalus.builtins.Builtins.unsafeDataAsList" -> DefaultFun.UnListData,
        "scalus.builtins.Builtins.unsafeDataAsMap" -> DefaultFun.UnMapData,
        "scalus.builtins.Builtins.unsafeDataAsB" -> DefaultFun.UnBData,
        "scalus.builtins.Builtins.unsafeDataAsI" -> DefaultFun.UnIData,
        "scalus.builtins.Builtins.sha2_256" -> DefaultFun.Sha2_256,
        "scalus.builtins.Builtins.trace" -> DefaultFun.Trace,
        "scalus.builtins.Builtins.indexByteString" -> DefaultFun.IndexByteString,
        "scalus.builtins.Builtins.consByteString" -> DefaultFun.ConsByteString,
        "scalus.builtins.Builtins.lengthOfByteString" -> DefaultFun.LengthOfByteString,
        "scalus.builtins.Builtins.lessThanInteger" -> DefaultFun.LessThanInteger,
        "scalus.builtins.Builtins.decodeUtf8" -> DefaultFun.DecodeUtf8,
        "scalus.builtins.Builtins.equalsInteger" -> DefaultFun.EqualsInteger,
        "scalus.builtins.Builtins.equalsByteString" -> DefaultFun.EqualsByteString,
        "scalus.builtins.Builtins.equalsString" -> DefaultFun.EqualsString,
        "scalus.builtins.Builtins.equalsData" -> DefaultFun.EqualsData
      )
      DefaultFunValues.get(tpe).map(SIR.Builtin.apply)
    }
  }

  def typeReprToDefaultUni(t: Type): DefaultUni =
    if t =:= converter.BigIntClassSymbol.typeRef then DefaultUni.Integer
    else if t =:= defn.StringClass.typeRef then DefaultUni.String
    else if t =:= defn.BooleanClass.typeRef then DefaultUni.Bool
    else if t =:= defn.UnitClass.typeRef then DefaultUni.Unit
    else if t =:= converter.DataClassSymbol.typeRef then DefaultUni.Data
    else if t =:= converter.ByteStringClassSymbol.typeRef then DefaultUni.ByteString
    else if t.isPair then
      val List(t1, t2) = t.dealias.argInfos
      DefaultUni.Pair(typeReprToDefaultUni(t1), typeReprToDefaultUni(t2))
    else if t.isList then
      val t1 = t.dealias.argInfos.head
      DefaultUni.List(typeReprToDefaultUni(t1))
    else
      report.error(s"Unsupported type $t")
      DefaultUni.Unit

  def compileExpr(env: immutable.HashSet[Symbol], tree: Tree)(using Context): SIR = {
    if compileConstant.isDefinedAt(tree) then
      val const = compileConstant(tree)
      SIR.Const(const)
    else
      tree match
        case If(cond, t, f) =>
          SIR.IfThenElse(compileToSIR(cond), compileToSIR(t), compileToSIR(f))
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

        // Boolean &&
        case Select(lhs, op) if lhs.tpe.widen =:= defn.BooleanType =>
          val lhsExpr = compileExpr(env, lhs)
          op match
            case nme.UNARY_! =>
              SIR.IfThenElse(
                lhsExpr,
                SIR.Const(scalus.uplc.Constant.Bool(false)),
                SIR.Const(scalus.uplc.Constant.Bool(true))
              )
            case nme.ZAND =>
              SIR.LamAbs(
                "rhs",
                SIR.IfThenElse(
                  lhsExpr,
                  SIR.Var(NamedDeBruijn("rhs")),
                  SIR.Const(scalus.uplc.Constant.Bool(false))
                )
              )
            case nme.ZOR =>
              SIR.LamAbs(
                "rhs",
                SIR.IfThenElse(
                  lhsExpr,
                  SIR.Const(scalus.uplc.Constant.Bool(true)),
                  SIR.Var(NamedDeBruijn("rhs"))
                )
              )
        // Data BUILTINS
        case bi: Select if BuiltinHelper.builtinFun(bi.symbol.showFullName).isDefined =>
          BuiltinHelper.builtinFun(bi.symbol.showFullName).get
        // BigInt stuff
        case Select(ident, op) if ident.tpe.widen =:= converter.BigIntClassSymbol.typeRef =>
          op match
            case nme.PLUS =>
              SIR.Apply(SIR.Builtin(DefaultFun.AddInteger), compileExpr(env, ident) )
            case nme.MINUS =>
              SIR.Apply(SIR.Builtin(DefaultFun.SubtractInteger), compileExpr(env, ident) )
            case nme.MUL =>
              SIR.Apply(SIR.Builtin(DefaultFun.MultiplyInteger), compileExpr(env, ident) )
            case nme.DIV =>
              SIR.Apply(SIR.Builtin(DefaultFun.DivideInteger), compileExpr(env, ident) )
            case nme.MOD =>
              SIR.Apply(SIR.Builtin(DefaultFun.RemainderInteger), compileExpr(env, ident) )
        // List BUILTINS
        case Select(lst, fun) if lst.isList =>
          fun.show match
            case "head" =>
              SIR.Apply(SIR.Builtin(DefaultFun.HeadList), compileExpr(env, lst))
            case "tail" =>
              SIR.Apply(SIR.Builtin(DefaultFun.TailList), compileExpr(env, lst))
            case "isEmpty" =>
              SIR.Apply(SIR.Builtin(DefaultFun.NullList), compileExpr(env, lst))
            case _ =>
              report.error(
                s"compileExpr: Unsupported list method $fun. Only head, tail and isEmpty are supported"
              )
              SIR.Error(s"Unsupported list method $fun")
        case TypeApply(Select(list, name), immutable.List(tpe))
            if name == termName("empty") && list.tpe =:= requiredModule(
              "scalus.builtins.List"
            ).typeRef =>
          val tpeE = typeReprToDefaultUni(tpe.tpe)
          SIR.Const(scalus.uplc.Constant.List(tpeE, Nil))
        case Apply(
              TypeApply(Select(list, name), immutable.List(tpe)),
              immutable.List(arg)
            ) if name == termName("::") && list.isList =>
          val argE = compileExpr(env, arg)
          SIR.Apply(SIR.Apply(SIR.Builtin(DefaultFun.MkCons), argE), compileExpr(env, list))
        case Apply(
              TypeApply(Select(list, nme.apply), immutable.List(tpe)),
              immutable.List(ex)
            ) if list.tpe =:= requiredModule("scalus.builtins.List").typeRef =>
          val tpeE = typeReprToDefaultUni(tpe.tpe)
          ex match
            case SeqLiteral(args, _) =>
              val allLiterals = args.forall(arg => compileConstant.isDefinedAt(arg))
              if allLiterals then
                report.echo("all literals")
                val lits = args.map(compileConstant)
                SIR.Const(scalus.uplc.Constant.List(tpeE, lits))
              else
                val nil = SIR.Const(scalus.uplc.Constant.List(tpeE, Nil))
                args.foldRight(nil) { (arg, acc) =>
                  SIR.Apply(
                    SIR.Apply(SIR.Builtin(DefaultFun.MkCons), compileExpr(env, arg)),
                    acc
                  )
                }
            case _ =>
              report.error(s"compileExpr: List is not supported yet ${ex}")
              SIR.Error("List is not supported")
        // Pair BUILTINS
        // PAIR
        case Select(pair, fun) if pair.isPair =>
          fun.show match
            case "fst" =>
              SIR.Apply(SIR.Builtin(DefaultFun.FstPair), compileExpr(env, pair))
            case "snd" =>
              SIR.Apply(SIR.Builtin(DefaultFun.SndPair), compileExpr(env, pair))
            case _ =>
              report.error(s"compileExpr: Unsupported pair function: $fun")
              SIR.Error(s"Unsupported pair function: $fun")
        case Apply(
              TypeApply(Select(pair, nme.apply), immutable.List(tpe1, tpe2)),
              immutable.List(a, b)
            ) if pair.tpe =:= requiredModule("scalus.builtins.Pair").typeRef =>
          // We can create a Pair by either 2 literals as (con pair...)
          // or 2 Data variables using MkPairData builtin
          if a.isLiteral && b.isLiteral then
            SIR.Const(
              scalus.uplc.Constant.Pair(compileConstant(a), compileConstant(b))
            )
          else if a.isData && b.isData then
            SIR.Apply(
              SIR.Apply(SIR.Builtin(DefaultFun.MkPairData), compileExpr(env, a)),
              compileExpr(env, b)
            )
          else
            report.error(
              s"""Builtin Pair can only be created either by 2 literals or 2 Data variables:
              |Pair[${tpe1.tpe.show},${tpe2.tpe.show}](${a.show}, ${b.show})
              |- ${a.show} literal: ${a.isLiteral}, data: ${a.isData}
              |- ${b.show} literal: ${b.isLiteral}, data: ${b.isData}
              |""".stripMargin
            )
            SIR.Error(
              s"""Builtin Pair can only be created either by 2 literals or 2 Data variables:
              |Pair[${tpe1.tpe.show},${tpe2.tpe.show}](${a.show}, ${b.show})
              |- ${a.show} literal: ${a.isLiteral}, data: ${a.isData}
              |- ${b.show} literal: ${b.isLiteral}, data: ${b.isData}
              |""".stripMargin
            )
        case tree @ Ident(a) =>
          compileIdentOrQualifiedSelect(env, tree)
        // f.apply(arg) => Apply(f, arg)
        case Apply(Select(f, nme.apply), args) if defn.isFunctionType(f.tpe.widen) =>
          val fE = compileExpr(env, f)
          val argsE = args.map(compileExpr(env, _))
          argsE.foldLeft(fE)((acc, arg) => SIR.Apply(acc, arg))
        // ignore asInstanceOf
        case TypeApply(Select(e, nme.asInstanceOf_), _) => compileExpr(env, e)
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
        case Inlined(_, bindings, expr) =>
          val r = compileBlock(env, bindings, expr)
          // val t = r.asTerm.show
          // report.info(s"Inlined: ${bindings}, ${expr.show}\n${t}", Position(SourceFile.current, globalPosition, 0))
          r
        case x =>
          report.error(s"Unsupported expression: ${x.show}\n$x", x.srcPos)
          SIR.Error("Unsupported expression")
  }

  def compileToSIR(tree: Tree)(using Context): SIR = {
    println(s"compileToSIR: ${tree}")
    val result = compileExpr(immutable.HashSet.empty, tree)
    val full = globalDefs.values.foldRight(result) {
      case (CompileDef.Compiled(b), acc) =>
        SIR.Let(b.recursivity, List(Binding(b.fullName, b.body)), acc)
      case (d, acc) =>
        report.error(s"Unexpected globalDefs state: $d")
        SIR.Error("Unexpected globalDefs state")

    }
    val dataDecls = globalDataDecls.foldRight(full) { case ((_, decl), acc) =>
      SIR.Decl(decl, acc)
    }
    dataDecls
  }

}
