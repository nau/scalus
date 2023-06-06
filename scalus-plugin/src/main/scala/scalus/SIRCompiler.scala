package scalus

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.*
import dotty.tools.dotc.util.SrcPos
import dotty.tools.io.ClassPath
import scalus.builtins.ByteString
import scalus.flat.DecoderState
import scalus.flat.EncoderState
import scalus.flat.Flat
import scalus.flat.FlatInstantces.given
import scalus.sir.Binding
import scalus.sir.Case
import scalus.sir.ConstrDecl
import scalus.sir.DataDecl
import scalus.sir.Recursivity
import scalus.sir.SIR
import scalus.uplc.DefaultFun
import scalus.uplc.DefaultUni

import java.net.URL
import scala.collection.immutable
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

case class Module(defs: List[Binding])
case class FullName(name: String)
object FullName:
  def apply(sym: Symbol)(using Context): FullName = FullName(sym.fullName.toString())

case class TopLevelBinding(fullName: FullName, recursivity: Recursivity, body: SIR)

case class B(name: String, symbol: Symbol, recursivity: Recursivity, body: SIR):
  def fullName(using Context) = FullName(symbol)

final class SIRCompiler(mode: scalus.Mode)(using ctx: Context) {
  import tpd.*
  type Env = immutable.HashSet[String]

  private val converter = new SIRConverter

  extension (t: Type)
    def isPair: Boolean =
      val r = t.typeSymbol.showFullName == "scalus.builtins.Pair"
      // println(s"$t is pair: $r, ${t.typeSymbol.showFullName}")
      r
    def isList: Boolean =
      t <:< requiredClass("scalus.builtins.List").typeRef.appliedTo(defn.AnyType)

  extension (self: Symbol)
    def caseFields: List[Symbol] =
      if !self.isClass then Nil
      else
        self.asClass.paramAccessors.collect {
          case sym if sym.is(dotty.tools.dotc.core.Flags.CaseAccessor) => sym.asTerm
        }

  extension (t: Tree) def isList: Boolean = t.tpe.isList

  extension (t: Tree) def isPair: Boolean = t.tpe.isPair

  extension (t: Tree) def isLiteral = compileConstant.isDefinedAt(t)
  extension (t: Tree) def isData = t.tpe <:< requiredClass("scalus.uplc.Data").typeRef

  enum CompileDef:
    case Compiling
    case Compiled(binding: TopLevelBinding)

  private val globalDefs: mutable.LinkedHashMap[FullName, CompileDef] = mutable.LinkedHashMap.empty
  private val globalDataDecls: mutable.LinkedHashMap[FullName, DataDecl] =
    mutable.LinkedHashMap.empty
  private val moduleDefsCache: mutable.Map[String, mutable.LinkedHashMap[FullName, SIR]] =
    mutable.LinkedHashMap.empty.withDefaultValue(mutable.LinkedHashMap.empty)

  private val CompileAnnot = requiredClassRef("scalus.Compile").symbol.asClass
  private val IngoreAnnot = requiredClassRef("scalus.Ignore").symbol.asClass

  private lazy val classLoader = makeClassLoader

  private def makeClassLoader(using Context): ClassLoader = {
    import scala.language.unsafeNulls

    val entries = ClassPath.expandPath(ctx.settings.classpath.value, expandStar = true)
    val urls = entries.map(cp => java.nio.file.Paths.get(cp).toUri.toURL).toArray
    val out = Option(
      ctx.settings.outputDir.value.toURL
    ) // to find classes in case of suspended compilation
    new java.net.URLClassLoader(urls ++ out.toList, getClass.getClassLoader)
  }

  def compileModule(tree: Tree): Unit = {
    def collectTypeDefs(tree: Tree): List[TypeDef] = {
      tree match
        case EmptyTree            => Nil
        case PackageDef(_, stats) => stats.flatMap(collectTypeDefs)
        case cd: TypeDef          =>
          // println(s"typedef ${cd.name}: ${cd.rhs.showIndented(2)}")
          if cd.symbol.hasAnnotation(CompileAnnot) then List(cd)
          else Nil
        case vd: ValDef =>
          // println(s"valdef $vd")
          Nil // module instance
        case Import(_, _) => Nil
    }

    val allTypeDefs = collectTypeDefs(tree)
    // println(allTypeDefs.map(td => s"${td.name} ${td.isClassDef}"))

    allTypeDefs.foreach(compileTypeDef)
  }

  private def compileTypeDef(td: TypeDef) = {
    report.echo(s"compiling to SIR: ${td.name}")

    val tpl = td.rhs.asInstanceOf[Template]
    val bindings = tpl.body.collect {
      // FIXME: hack for derived methods
      case dd: DefDef
          if !dd.symbol.flags.is(Flags.Synthetic)
            && !dd.symbol.name.startsWith("derived")
            && !dd.symbol.hasAnnotation(IngoreAnnot) =>
        compileStmt(immutable.HashSet.empty, dd, isGlobalDef = true)
      case vd: ValDef
          if !vd.symbol.flags.isOneOf(Flags.Synthetic | Flags.Case)
            && !vd.symbol.name.startsWith("derived")
            && !vd.symbol.hasAnnotation(IngoreAnnot) =>
        // println(s"valdef: ${vd.symbol.fullName}")
        compileStmt(immutable.HashSet.empty, vd, isGlobalDef = true)
    }
    val module = Module(bindings.map(b => Binding(b.fullName.name, b.body)))
    report.echo(s"module ${td.name} definitions: ${bindings.map(_.name).mkString(", ")}")
    writeModule(module, td.symbol.fullName.toString())
  }

  private def writeModule(module: Module, className: String) = {
    val suffix = ".sir"
    val outputDirectory = ctx.settings.outputDir.value
    val pathParts = className.split('.')
    val dir = pathParts.init.foldLeft(outputDirectory)(_.subdirectoryNamed(_))
    val filename = pathParts.last
    val output = dir.fileNamed(filename + suffix).bufferedOutput
    val fl = summon[Flat[Module]]
    val enc = EncoderState(fl.bitSize(module) / 8 + 1)
    flat.encode(module, enc)
    enc.filler()
    output.write(enc.buffer)
    output.close()
  }

  case class AdtTypeInfo(
      constructorTypeSymbol: Symbol,
      dataTypeSymbol: Symbol,
      constructors: List[Symbol]
  )

  private def getAdtInfoFromConstroctorType(constrTpe: Type): AdtTypeInfo = {
    /* We support these cases:
        1. case class Foo(a: Int, b: String)
        2. case object Bar
        3. enum Base { case A ...}
        4. enum Base { case B(a, b) }
        5. sealed abstract class Base; object Base { case object A extends Base }
        6. sealed abstract class Base; object Base { case class B(a: Int, b: String) extends Base }
        7. scala.Tuple2

     */
    val typeSymbol = constrTpe.widen.dealias.typeSymbol
    // println(s"getAdtInfoFromConstroctorType: ${typeSymbol.showFullName}, $constrTpe")
    // look for a base `sealed abstract class`. If it exists, we are in case 5 or 6
    val adtBaseType = constrTpe.baseClasses.find(b =>
      // println(s"base class: ${b.show} ${b.flags.flagsString}")
      b.flags.isAllOf(Flags.Sealed | Flags.Abstract) && !b.flags.is(Flags.Trait)
    )

    val info =
      if typeSymbol.showFullName == "scala.Tuple2"
      then AdtTypeInfo(typeSymbol, typeSymbol, List(typeSymbol))
      else
        adtBaseType match
          case None => // case 1 or 2
            AdtTypeInfo(typeSymbol, typeSymbol, List(typeSymbol))
          case Some(baseClassSymbol) if constrTpe.isSingleton => // case 3, 5
            AdtTypeInfo(constrTpe.termSymbol, baseClassSymbol, baseClassSymbol.children)
          case Some(baseClassSymbol) => // case 4, 6
            AdtTypeInfo(typeSymbol, baseClassSymbol, baseClassSymbol.children)
    /* report.echo(
      s"getAdtInfoFromConstroctorType: ${constrTpe.show}: ${typeSymbol.showFullName} ${adtBaseType} $info"
    ) */
    info
  }

  private def primaryConstructorParams(typeSymbol: Symbol): List[Symbol] = {
    val fields = typeSymbol.primaryConstructor.paramSymss.flatten.filter(s => s.isTerm)
    // debugInfo(s"caseFields: ${typeSymbol.fullName} $fields")
    fields
  }

  private def findAndReadModuleOfSymbol(moduleName: String): Option[Module] = {
    val filename = moduleName.replace('.', '/') + ".sir"
    // println(s"findAndReadModuleOfSymbol: ${filename}")
    // read the file from the classpath
    val resource = classLoader.getResourceAsStream(filename)
    if resource != null then
      val buffer = resource.readAllBytes()
      val dec = DecoderState(buffer)
      val module = flat.decode[Module](dec)
      resource.close()
      Some(module)
    else None
  }

  private def compileNewConstructor(
      env: Env,
      tpe: Type,
      args: immutable.List[Tree]
  ): SIR = {

    // val typeSymbol = tpe.typeSymbol
    // debugInfo(s"compileNewConstructor0")
    /* report.echo(
      s"compileNewConstructor1 ${typeSymbol} singleton ${tpe.isSingleton} companion: ${typeSymbol.maybeOwner.companionClass} " +
      s"${typeSymbol.children} widen: ${tpe.widen.typeSymbol}, widen.children: ${tpe.widen.typeSymbol.children} ${typeSymbol.maybeOwner.companionClass.children}"
      ) */

    val adtInfo = getAdtInfoFromConstroctorType(tpe)
    // report.echo(s"compileNewConstructor1 ${tpe.show} base type: ${adtInfo}")

    val argsE = args.map(compileExpr(env, _))
    val constrName = adtInfo.constructorTypeSymbol.name.show
    // sort by name to get a stable order
    val sortedConstructors = adtInfo.constructors.sortBy(_.name.show)
    val constrDecls = sortedConstructors.map { sym =>
      val params = primaryConstructorParams(sym).map(_.name.show)
      scalus.sir.ConstrDecl(sym.name.show, params)
    }
    val dataName = adtInfo.dataTypeSymbol.name.show
    // debugInfo(s"compileNewConstructor2: dataTypeSymbol $dataTypeSymbol, dataName $dataName, constrName $constrName, children ${constructors}")
    val dataDecl = globalDataDecls.get(FullName(adtInfo.dataTypeSymbol)) match
      case Some(decl) => decl
      case None =>
        val decl = scalus.sir.DataDecl(dataName, constrDecls)
        globalDataDecls.addOne(FullName(adtInfo.dataTypeSymbol) -> decl)
        decl
    // constructor body as: constr arg1 arg2 ...
    SIR.Constr(constrName, dataDecl, argsE)
  }

  // Parameterless case class constructor of an enum
  private def isConstructorVal(symbol: Symbol, tpe: Type): Boolean =
    /* println(
        s"isConstructorVal: ${symbol.flags.isAllOf(Flags.EnumCase)} $symbol: ${tpe.show} <: ${tpe.widen.show}, ${symbol.flagsString}"
      )  */
    symbol.flags.isAllOf(Flags.EnumCase)

  def traverseAndLink(sir: SIR, srcPos: SrcPos): Unit = sir match
    case SIR.ExternalVar(moduleName, name) if !globalDefs.contains(FullName(name)) =>
      linkDefinition(moduleName, FullName(name), srcPos)
    case SIR.Let(recursivity, bindings, body) =>
      bindings.foreach(b => traverseAndLink(b.value, srcPos))
      traverseAndLink(body, srcPos)
    case SIR.LamAbs(name, term) => traverseAndLink(term, srcPos)
    case SIR.Apply(f, arg) =>
      traverseAndLink(f, srcPos)
      traverseAndLink(arg, srcPos)
    case SIR.And(lhs, rhs) =>
      traverseAndLink(lhs, srcPos)
      traverseAndLink(rhs, srcPos)
    case SIR.Or(lhs, rhs) =>
      traverseAndLink(lhs, srcPos)
      traverseAndLink(rhs, srcPos)
    case SIR.Not(term) => traverseAndLink(term, srcPos)
    case SIR.IfThenElse(cond, t, f) =>
      traverseAndLink(cond, srcPos)
      traverseAndLink(t, srcPos)
      traverseAndLink(f, srcPos)
    case SIR.Decl(data, term) => traverseAndLink(term, srcPos)
    case SIR.Constr(name, data, args) =>
      globalDataDecls.put(FullName(data.name), data)
      args.foreach(a => traverseAndLink(a, srcPos))
    case SIR.Match(scrutinee, cases) =>
      traverseAndLink(scrutinee, srcPos)
      cases.foreach(c => traverseAndLink(c.body, srcPos))
    case _ => ()

  private def findAndLinkDefinition(
      defs: collection.Map[FullName, SIR],
      fullName: FullName,
      srcPos: SrcPos
  ): Option[SIR] = {
    // println(s"findAndLinkDefinition: looking for ${fullName.name}")
    defs.get(fullName).map { sir =>
      globalDefs.update(fullName, CompileDef.Compiling)
      traverseAndLink(sir, srcPos)
      globalDefs.remove(fullName)
      globalDefs.update(
        fullName,
        CompileDef.Compiled(TopLevelBinding(fullName, Recursivity.Rec, sir))
      )
      sir
    }
  }

  private def linkDefinition(moduleName: String, fullName: FullName, srcPos: SrcPos): SIR = {
    // println(s"linkDefinition: ${fullName}")
    val defn = moduleDefsCache.get(moduleName) match
      case Some(defs) =>
        findAndLinkDefinition(defs, fullName, srcPos)
      case None =>
        findAndReadModuleOfSymbol(moduleName).flatMap { case m @ Module(defs) =>
          // println(s"Loaded module ${moduleName}, defs: ${defs}")
          val defsMap = mutable.LinkedHashMap.from(defs.map(d => FullName(d.name) -> d.value))
          moduleDefsCache.put(moduleName, defsMap)
          findAndLinkDefinition(defsMap, fullName, srcPos)
        }
    defn match
      case Some(d) =>
        // println(s"Found definition of ${fullName.name}")
        SIR.Var(fullName.name)
      case None =>
        report.error(s"Symbol ${fullName.name} is not found", srcPos)
        SIR.Error(s"Symbol ${fullName.name} not defined")
  }

  private def compileIdentOrQualifiedSelect(env: Env, e: Tree): SIR = {
    val name = e.symbol.name.show
    val fullName = FullName(e.symbol)
    val isInLocalEnv = env.contains(name)
    val isInGlobalEnv = globalDefs.contains(fullName)
    // println( s"compileIdentOrQualifiedSelect1: ${e.symbol} $name $fullName, term: ${e.show}, loc/glob: $isInLocalEnv/$isInGlobalEnv, env: ${env}" )
    (isInLocalEnv, isInGlobalEnv) match
      // global def, self reference, use the name
      case (true, true) => SIR.Var(e.symbol.fullName.toString())
      // local def, use the name
      case (true, false) => SIR.Var(e.symbol.name.show)
      // global def, use full name
      case (false, true) => SIR.Var(e.symbol.fullName.toString())
      case (false, false) =>
        mode match
          case scalus.Mode.Compile =>
            // println( s"external var: module ${e.symbol.owner.fullName.toString()}, ${e.symbol.fullName.toString()}" )
            SIR.ExternalVar(e.symbol.owner.fullName.toString(), e.symbol.fullName.toString())
          case scalus.Mode.Link =>
            if e.symbol.defTree == EmptyTree then
              linkDefinition(e.symbol.owner.fullName.toString(), fullName, e.symbol.sourcePos)
            else
              // println(s"compileIdentOrQualifiedSelect2: ${e.symbol} ${e.symbol.defTree}")
              // remember the symbol to avoid infinite recursion
              globalDefs.update(fullName, CompileDef.Compiling)
              // println(s"Tree of ${e}: ${e.tpe} isList: ${e.isList}")
              // debugInfo(s"Tree of ${e.symbol}: ${e.symbol.tree.show}\n${e.symbol.tree}")
              val b = compileStmt(immutable.HashSet.empty, e.symbol.defTree, isGlobalDef = true)
              // remove the symbol from the linked hash map so the order of the definitions is preserved
              globalDefs.remove(fullName)
              traverseAndLink(b.body, e.symbol.sourcePos)
              globalDefs.update(
                fullName,
                CompileDef.Compiled(TopLevelBinding(fullName, b.recursivity, b.body))
              )
              SIR.Var(e.symbol.fullName.toString())
  }

  private def compileStmt(env: Env, stmt: Tree, isGlobalDef: Boolean = false): B = {
    // report.echo(s"compileStmt  ${stmt.show} in ${env}")
    stmt match
      case vd @ ValDef(name, _, _) =>
        val bodyExpr = compileExpr(env, vd.rhs)
        B(name.show, vd.symbol, Recursivity.NonRec, bodyExpr)
      case dd @ DefDef(name, paramss, tpe, _) =>
        // report.echo(s"compileStmt DefDef ${dd.name.show}, ${dd.symbol.flags.flagsString}")
        val params = paramss.flatten.collect({ case vd: ValDef => vd })
        val names =
          if params.isEmpty then List("_") /* Param for () argument */
          else params.map { case v: ValDef => v.symbol.name.show }
        val body = dd.rhs
        val selfName = if isGlobalDef then FullName(dd.symbol).name else dd.symbol.name.show
        val bE = compileExpr(env ++ names + selfName, body)
        val bodyExpr: scalus.sir.SIR =
          names.foldRight(bE) { (name, acc) =>
            SIR.LamAbs(name, acc)
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

  private def compileBlock(env: Env, stmts: immutable.List[Tree], expr: Tree): SIR = {
    val exprs = ListBuffer.empty[B]
    val exprEnv = stmts.foldLeft(env) {
      case (env, _: Import)  => env // ignore local imports
      case (env, _: TypeDef) => env // ignore local type definitions
      case (env, stmt) =>
        val bind = compileStmt(env, stmt)
        exprs += bind
        env + bind.name
    }
    val exprExpr = compileExpr(exprEnv, expr)
    exprs.foldRight(exprExpr) { (bind, expr) =>
      SIR.Let(bind.recursivity, List(Binding(bind.name, bind.body)), expr)
    }
  }

  /* Sometimes the compiler leaves Inlined nodes,
   * which we need to skip to get to the actual expression
   * otherwise,say, constants are not compiled correctly
   */
  object SkipInline {
    def unapply(expr: Tree): Some[Tree] =
      expr match
        case Inlined(EmptyTree, Nil, t) => unapply(t)
        case _                          => Some(expr)
  }

  private def compileConstant: PartialFunction[Tree, scalus.uplc.Constant] = {
    case l @ Literal(c: Constant) =>
      c.tag match
        case Constants.BooleanTag => scalus.uplc.Constant.Bool(c.booleanValue)
        case Constants.StringTag  => scalus.uplc.Constant.String(c.stringValue)
        case Constants.UnitTag    => scalus.uplc.Constant.Unit
        case Constants.IntTag =>
          report.error(
            s"Scalus: Int literals are not supported. Try BigInt(${c.intValue}) instead",
            l.srcPos
          )
          scalus.uplc.Constant.Unit
        case _ =>
          report.error(s"Unsupported constant type $c");
          scalus.uplc.Constant.Unit

    case e @ Literal(_) =>
      report.error(s"compileExpr: Unsupported literal ${e.show}\n$e", e.srcPos)
      scalus.uplc.Constant.Unit
    case t @ Apply(bigintApply, List(SkipInline(Literal(c))))
        if bigintApply.symbol.showFullName == "scala.math.BigInt.apply" =>
      c.tag match
        case Constants.IntTag =>
          scalus.uplc.Constant.Integer(BigInt(c.intValue))
        case Constants.StringTag =>
          scalus.uplc.Constant.Integer(BigInt(c.stringValue))
        case _ =>
          report.error(s"Unsupported constant type $c", t.srcPos);
          scalus.uplc.Constant.Unit
    case Apply(i, List(Literal(c))) if i.symbol.showFullName == "scala.math.BigInt.int2bigInt" =>
      scalus.uplc.Constant.Integer(BigInt(c.intValue))
    case expr if expr.symbol.showFullName == "scalus.builtins.ByteString.empty" =>
      scalus.uplc.Constant.ByteString(scalus.builtins.ByteString.empty)
    case Apply(expr, List(Literal(c)))
        if expr.symbol.showFullName == "scalus.builtins.ByteString.fromHex" =>
      scalus.uplc.Constant.ByteString(scalus.builtins.ByteString.fromHex(c.stringValue))
    // hex"deadbeef" as ByteString
    case Apply(
          Select(
            Apply(
              stringInterpolators,
              List(
                Apply(
                  Select(stringContext, nme.apply),
                  List(SeqLiteral(List(Literal(const)), _))
                )
              )
            ),
            hex
          ),
          List(SeqLiteral(Nil, _))
        )
        if stringInterpolators.symbol.showFullName == "scalus.builtins.ByteString.StringInterpolators"
          && stringContext.symbol.showFullName == "scala.StringContext" && hex == termName("hex") &&
          const.tag == Constants.StringTag =>
      scalus.uplc.Constant.ByteString(scalus.builtins.ByteString.fromHex(const.stringValue))
  }

  private def typeReprToDefaultUni(t: Type, pos: SrcPos): DefaultUni =
    if t =:= converter.BigIntClassSymbol.typeRef then DefaultUni.Integer
    else if t =:= defn.StringClass.typeRef then DefaultUni.String
    else if t =:= defn.BooleanClass.typeRef then DefaultUni.Bool
    else if t =:= defn.UnitClass.typeRef then DefaultUni.Unit
    else if t =:= converter.DataClassSymbol.typeRef then DefaultUni.Data
    else if t =:= converter.ByteStringClassSymbol.typeRef then DefaultUni.ByteString
    else if t.isPair then
      val List(t1, t2) = t.dealias.argInfos
      DefaultUni.Pair(typeReprToDefaultUni(t1, pos), typeReprToDefaultUni(t2, pos))
    else if t.isList then
      val t1 = t.dealias.argInfos.head
      DefaultUni.List(typeReprToDefaultUni(t1, pos))
    else
      report.error(s"Unsupported type $t", pos)
      DefaultUni.Unit

  private def compileBigIntOps(env: Env, lhs: Tree, op: Name, rhs: Tree): SIR =
    op match
      case nme.PLUS =>
        SIR.Apply(
          SIR.Apply(SIR.Builtin(DefaultFun.AddInteger), compileExpr(env, lhs)),
          compileExpr(env, rhs)
        )
      case nme.MINUS =>
        SIR.Apply(
          SIR.Apply(SIR.Builtin(DefaultFun.SubtractInteger), compileExpr(env, lhs)),
          compileExpr(env, rhs)
        )
      case nme.MUL =>
        SIR.Apply(
          SIR.Apply(SIR.Builtin(DefaultFun.MultiplyInteger), compileExpr(env, lhs)),
          compileExpr(env, rhs)
        )
      case nme.DIV =>
        SIR.Apply(
          SIR.Apply(SIR.Builtin(DefaultFun.DivideInteger), compileExpr(env, lhs)),
          compileExpr(env, rhs)
        )
      case nme.MOD =>
        SIR.Apply(
          SIR.Apply(SIR.Builtin(DefaultFun.RemainderInteger), compileExpr(env, lhs)),
          compileExpr(env, rhs)
        )
      case nme.LT =>
        SIR.Apply(
          SIR.Apply(SIR.Builtin(DefaultFun.LessThanInteger), compileExpr(env, lhs)),
          compileExpr(env, rhs)
        )
      case nme.LE =>
        SIR.Apply(
          SIR.Apply(SIR.Builtin(DefaultFun.LessThanEqualsInteger), compileExpr(env, lhs)),
          compileExpr(env, rhs)
        )
      case nme.GT =>
        SIR.Apply(
          SIR.Apply(SIR.Builtin(DefaultFun.LessThanInteger), compileExpr(env, rhs)),
          compileExpr(env, lhs)
        )
      case nme.GE =>
        SIR.Apply(
          (SIR.Apply(SIR.Builtin(DefaultFun.LessThanEqualsInteger), compileExpr(env, rhs))),
          compileExpr(env, lhs)
        )
      case nme.EQ =>
        SIR.Apply(
          SIR.Apply(SIR.Builtin(DefaultFun.EqualsInteger), compileExpr(env, lhs)),
          compileExpr(env, rhs)
        )
      case nme.NE =>
        SIR.Not(
          SIR.Apply(
            SIR.Apply(SIR.Builtin(DefaultFun.EqualsInteger), compileExpr(env, lhs)),
            compileExpr(env, rhs)
          )
        )
      case _ =>
        report.error(
          s"Unsupported BigInt operation $op. Only +, -, *, /, %, <, <=, >, >=, ==, != are supported",
          lhs.srcPos
        )
        SIR.Error("Unsupported BigInt operation $op.")

  /*
    enum A:
      case C0
      case C1(a)
      case C2(b, c)
      case C3(a, b, c)
    compiles to:
    Decl(DataDecl("A", List(ConstrDecl("C0", List()), ConstrDecl("C1", List("a")), ConstrDecl("C2", List("b", "c")))), ...)

    c ==> Constr("C0", constrDecl, List())

    c match
      case C0 => 0
      case C1(a) => 1
      case C2(b, c) => 2
      case _ => 3
    compiles to:
    Match(c, List(
      Case(C0, Nil, 0),
      Case(C1, List(a), 1),
      Case(C2, List(b, c), 2),
      Case(C3, List(_, _, _), 3)
    )
   */
  private def compileMatch(tree: Match, env: Env): SIR = {
    val Match(matchTree, cases) = tree
    val typeSymbol = matchTree.tpe.widen.dealias.typeSymbol
    val adtInfo = getAdtInfoFromConstroctorType(matchTree.tpe)
    // report.echo(s"Match: ${typeSymbol} ${typeSymbol.children} $adtInfo", tree.srcPos)

    def constructCase(
        constrSymbol: Symbol,
        bindings: List[String],
        rhs: SIR
    ): scalus.sir.Case = {
      val params = primaryConstructorParams(constrSymbol).map(_.name.show)
      val constrDecl = scalus.sir.ConstrDecl(constrSymbol.name.show, params)

      scalus.sir.Case(constrDecl, bindings, rhs)
    }

    def compileBinding(pat: Tree): String = {
      pat match
        case b @ Bind(name, Ident(nme.WILDCARD)) => b.symbol.name.show
        case Ident(nme.WILDCARD)                 => "_"
        case p =>
          report.error(s"Unsupported binding: ${p}", p.srcPos)
          s"Unsupported binding: ${p}"
    }

    enum SirCase:
      case Case(constructorSymbol: Symbol, bindings: List[String], rhs: SIR)
      case Wildcard(rhs: SIR, srcPos: SrcPos)
      case Error(msg: String, srcPos: SrcPos)

    def scalaCaseDefToSirCase(c: CaseDef): SirCase = c match
      case CaseDef(_, guard, _) if !guard.isEmpty =>
        SirCase.Error(s"Guards are not supported in match expressions", guard.srcPos)
      // this case is for matching on a case class
      case CaseDef(UnApply(_, _, pats), _, rhs) =>
        // report.error(s"Case: ${fun}, pats: ${pats}, rhs: $rhs", t.pos)
        val bindings = pats.map(compileBinding)
        val rhsE = compileExpr(env ++ bindings, rhs)
        SirCase.Case(typeSymbol, bindings, rhsE)
      // this case is for matching on an enum
      case CaseDef(Typed(UnApply(_, _, pats), constrTpe), _, rhs) =>
        // report.info(s"Case: ${inner}, tpe ${constrTpe.tpe.widen.show}", t.pos)
        val bindings = pats.map(compileBinding)
        val rhsE = compileExpr(env ++ bindings, rhs)
        SirCase.Case(constrTpe.tpe.typeSymbol, bindings, rhsE)
      // case object
      case CaseDef(pat, _, rhs) if pat.symbol.is(Flags.Case) =>
        val rhsE = compileExpr(env, rhs)
        // no-arg constructor, it's a Val, so we use termSymbol
        SirCase.Case(pat.tpe.termSymbol, Nil, rhsE)
      // case _ => rhs, wildcard pattern, must be the last case
      case CaseDef(Ident(nme.WILDCARD), _, rhs) =>
        val rhsE = compileExpr(env, rhs)
        SirCase.Wildcard(rhsE, c.srcPos)
      case a =>
        val cs = cases
          .map { case CaseDef(pat, _, _) => pat.toString() + " " + pat.symbol.flagsString }
          .mkString("\n")
        SirCase.Error(
          s"Unsupported match expression: ${a.show}\n$a\n${matchTree.tpe.typeSymbol}\n${cs}",
          matchTree.srcPos
        )

    val matchExpr = compileExpr(env, matchTree)
    val sirCases = cases.map(scalaCaseDefToSirCase)

    // 1. If we have a wildcard case, it must be the last one
    // 2. Validate we don't have any errors
    // 3. Convert Wildcard to the rest of the cases/constructors
    // 4. Ensure we cover all constructors
    // 5. Sort the cases by constructor name

    var idx = 0
    val iter = sirCases.iterator
    val allConstructors = adtInfo.constructors.toSet
    val matchedConstructors = mutable.HashSet.empty[Symbol]
    val expandedCases = mutable.ArrayBuffer.empty[scalus.sir.Case]

    while iter.hasNext do
      iter.next() match
        case SirCase.Case(constructorSymbol, bindings, rhs) =>
          matchedConstructors += constructorSymbol // collect all matched constructors
          expandedCases += constructCase(constructorSymbol, bindings, rhs)
        case SirCase.Wildcard(rhs, srcPos) =>
          // If we have a wildcard case, it must be the last one
          if idx != sirCases.length - 1 then
            report.error(s"Wildcard case must be the last and only one in match expression", srcPos)
          else
            // Convert Wildcard to the rest of the cases/constructors
            val missingConstructors = allConstructors -- matchedConstructors
            missingConstructors.foreach { constr =>
              val bindings = primaryConstructorParams(constr).map(_ => "_")
              // TODO: extract rhs to a let binding before the match
              // so we don't have to repeat it for each case
              expandedCases += constructCase(constr, bindings, rhs)
              matchedConstructors += constr // collect all matched constructors
            }
        case SirCase.Error(msg, srcPos) => report.error(msg, srcPos)

      idx += 1
    end while
    // Ensure we cover all constructors
    val missingConstructors = allConstructors -- matchedConstructors
    if missingConstructors.nonEmpty then
      val missing = missingConstructors.map(_.name).toBuffer.sorted.mkString(", ")
      report.error(s"Missing cases for constructors: ${missing}", matchTree.srcPos)

    // Sort the cases by constructor name to ensure we have a deterministic order
    val sortedCases = expandedCases.sortBy(_.constr.name).toList
    SIR.Match(matchExpr, sortedCases)
  }

  private def compileExpr(env: Env, tree: Tree)(using Context): SIR = {
    // println(s"compileExpr: ${tree.showIndented(2)}, env: $env")
    if compileConstant.isDefinedAt(tree) then
      val const = compileConstant(tree)
      SIR.Const(const)
    else
      tree match
        case If(cond, t, f) =>
          SIR.IfThenElse(compileExpr(env, cond), compileExpr(env, t), compileExpr(env, f))

        case m: Match => compileMatch(m, env)

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
        case Select(lhs, op) if lhs.tpe.widen =:= defn.BooleanType && op == nme.UNARY_! =>
          val lhsExpr = compileExpr(env, lhs)
          SIR.Not(lhsExpr)
        case Apply(Select(lhs, op), List(rhs))
            if lhs.tpe.widen =:= defn.BooleanType && op == nme.ZAND =>
          val lhsExpr = compileExpr(env, lhs)
          val rhsExpr = compileExpr(env, rhs)
          SIR.And(lhsExpr, rhsExpr)
        case Apply(Select(lhs, op), List(rhs))
            if lhs.tpe.widen =:= defn.BooleanType && op == nme.ZOR =>
          val lhsExpr = compileExpr(env, lhs)
          val rhsExpr = compileExpr(env, rhs)
          SIR.Or(lhsExpr, rhsExpr)
        // Data BUILTINS
        case bi: Select if BuiltinHelper.builtinFun(bi.symbol.showFullName).isDefined =>
          BuiltinHelper.builtinFun(bi.symbol.showFullName).get
        case bi: Ident if BuiltinHelper.builtinFun(bi.symbol.showFullName).isDefined =>
          BuiltinHelper.builtinFun(bi.symbol.showFullName).get
        // BigInt stuff
        case Apply(Select(lhs, op), List(rhs))
            if lhs.tpe.widen =:= converter.BigIntClassSymbol.typeRef =>
          compileBigIntOps(env, lhs, op, rhs)
        case Select(expr, op)
            if expr.tpe.widen =:= converter.BigIntClassSymbol.typeRef && op == nme.UNARY_- =>
          SIR.Apply(
            SIR.Apply(
              SIR.Builtin(DefaultFun.SubtractInteger),
              SIR.Const(scalus.uplc.Constant.Integer(BigInt(0)))
            ),
            compileExpr(env, expr)
          )
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
        case tree @ TypeApply(Select(list, name), immutable.List(tpe))
            if name == termName("empty") && list.tpe =:= requiredModule(
              "scalus.builtins.List"
            ).typeRef =>
          val tpeE = typeReprToDefaultUni(tpe.tpe, tree.srcPos)
          SIR.Const(scalus.uplc.Constant.List(tpeE, Nil))
        case Apply(
              TypeApply(Select(list, name), immutable.List(tpe)),
              immutable.List(arg)
            ) if name == termName("::") && list.isList =>
          val argE = compileExpr(env, arg)
          SIR.Apply(SIR.Apply(SIR.Builtin(DefaultFun.MkCons), argE), compileExpr(env, list))
        case tree @ Apply(
              TypeApply(Select(list, nme.apply), immutable.List(tpe)),
              immutable.List(ex)
            ) if list.tpe =:= requiredModule("scalus.builtins.List").typeRef =>
          val tpeE = typeReprToDefaultUni(tpe.tpe, list.srcPos)
          ex match
            case SeqLiteral(args, _) =>
              val allLiterals = args.forall(arg => compileConstant.isDefinedAt(arg))
              if allLiterals then
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
              report.error(s"compileExpr: List is not supported yet ${ex}", tree.srcPos)
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
              report.error(s"compileExpr: Unsupported pair function: $fun", tree.srcPos)
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
            val msg = s"""Builtin Pair can only be created either by 2 literals or 2 Data variables:
                         |Pair[${tpe1.tpe.show},${tpe2.tpe.show}](${a.show}, ${b.show})
                         |- ${a.show} literal: ${a.isLiteral}, data: ${a.isData}
                         |- ${b.show} literal: ${b.isLiteral}, data: ${b.isData}
                         |""".stripMargin
            report.error(msg, tree.srcPos)
            SIR.Error(msg)
        // new Constr(args)
        case Apply(TypeApply(con @ Select(f, nme.CONSTRUCTOR), _), args) =>
          compileNewConstructor(env, f.tpe, args)
        case Apply(con @ Select(f, nme.CONSTRUCTOR), args) =>
          compileNewConstructor(env, f.tpe, args)
        // (a, b) as scala.Tuple2.apply(a, b)
        // we need to special-case it because we use scala-library 2.13.x
        // which does not include TASTy so we can't access the method body
        case Apply(TypeApply(app @ Select(f, nme.apply), _), args)
            if app.symbol.fullName.show == "scala.Tuple2$.apply" =>
          compileNewConstructor(env, tree.tpe, args)
        case Apply(app @ Select(f, nme.apply), args)
            if app.symbol.fullName.show == "scala.Tuple2$.apply" =>
          compileNewConstructor(env, tree.tpe, args)
        // f.apply(arg) => Apply(f, arg)
        case Apply(Select(f, nme.apply), args) if defn.isFunctionType(f.tpe.widen) =>
          val fE = compileExpr(env, f)
          val argsE = args.map(compileExpr(env, _))
          argsE.foldLeft(fE)((acc, arg) => SIR.Apply(acc, arg))
        case Ident(a) =>
          if isConstructorVal(tree.symbol, tree.tpe) then compileNewConstructor(env, tree.tpe, Nil)
          else compileIdentOrQualifiedSelect(env, tree)
        // case class User(name: String, age: Int)
        // val user = User("John", 42) => \u - u "John" 42
        // user.name => \u name age -> name
        case sel @ Select(obj, ident) =>
          /* report.echo(
            s"select: Select: ${sel.show}: ${obj.tpe.widen.show} . ${ident}, isList: ${obj.isList}",
            sel.srcPos
          ) */
          val ts = obj.tpe.widen.dealias.typeSymbol
          lazy val fieldIdx = ts.caseFields.indexOf(sel.symbol)
          if ts.isClass && fieldIdx >= 0 then
            val lhs = compileExpr(env, obj)
            val lam = primaryConstructorParams(ts).foldRight(SIR.Var(ident.show)) { case (f, acc) =>
              SIR.LamAbs(f.name.show, acc)
            }
            SIR.Apply(lhs, lam)
          // else if obj.symbol.isPackageDef then
          // compileExpr(env, obj)
          else if isConstructorVal(tree.symbol, tree.tpe) then
            compileNewConstructor(env, tree.tpe, Nil)
          else compileIdentOrQualifiedSelect(env, tree)
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
        case Return(_, _) =>
          report.error(s"return expression is not supported: ${tree.show}", tree.srcPos)
          SIR.Error("Unsupported return statement")
        case Assign(lhs, rhs) =>
          report.error(s"Variable assignment is not supported: ${tree.show}", tree.srcPos)
          SIR.Error("Unsupported assign expression")
        case Try(_, _, _) =>
          report.error(s"try expression is not supported: ${tree.show}", tree.srcPos)
          SIR.Error("Unsupported try expression")
        case WhileDo(cond, body) =>
          report.error(s"while statement is not supported: ${tree.show}", tree.srcPos)
          SIR.Error("Unsupported while expression")
        case x =>
          report.error(s"Unsupported expression: ${x.show}\n$x", x.srcPos)
          SIR.Error("Unsupported expression")
  }

  def compileToSIR(tree: Tree)(using Context): SIR = {
    // println(s"compileToSIR: ${tree}")
    val result = compileExpr(immutable.HashSet.empty, tree)
    val full = globalDefs.values.foldRight(result) {
      case (CompileDef.Compiled(b), acc) =>
        SIR.Let(b.recursivity, List(Binding(b.fullName.name, b.body)), acc)
      case (d, acc) =>
        report.error(s"Unexpected globalDefs state: $d\n$globalDefs", tree.srcPos)
        SIR.Error("Unexpected globalDefs state")

    }
    val dataDecls = globalDataDecls.foldRight(full) { case ((_, decl), acc) =>
      SIR.Decl(decl, acc)
    }
    dataDecls
  }

}
