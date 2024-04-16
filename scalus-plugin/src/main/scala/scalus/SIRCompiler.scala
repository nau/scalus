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
import dotty.tools.dotc.core.Types.{MethodType, Type}
import dotty.tools.dotc.core.*
import dotty.tools.dotc.util.SrcPos
import dotty.tools.io.ClassPath
import scalus.builtin.ByteString
import scalus.flat.DecoderState
import scalus.flat.EncoderState
import scalus.flat.Flat
import scalus.flat.FlatInstantces.given
import scalus.sir.Binding
import scalus.sir.ConstrDecl
import scalus.sir.DataDecl
import scalus.sir.Module
import scalus.sir.Recursivity
import scalus.sir.SIR
import scalus.uplc.DefaultFun
import scalus.uplc.DefaultUni

import java.net.URL
import scala.collection.immutable
import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import scala.annotation.unused

case class FullName(name: String)
object FullName:
    def apply(sym: Symbol)(using Context): FullName = FullName(sym.fullName.toString())

case class TopLevelBinding(fullName: FullName, recursivity: Recursivity, body: SIR)

case class B(name: String, symbol: Symbol, recursivity: Recursivity, body: SIR):
    def fullName(using Context) = FullName(symbol)

case class AdtTypeInfo(
    constructorTypeSymbol: Symbol,
    dataTypeSymbol: Symbol,
    constructors: List[Symbol]
)

final class SIRCompiler(mode: scalus.Mode)(using ctx: Context) {
    import tpd.*
    type Env = HashSet[String]

    val SirVersion = (0, 0)

    private val converter = new SIRConverter
    private val builtinsHelper = new BuiltinHelper
    private val PairSymbol = requiredClass("scalus.builtin.Pair")
    private val ScalusBuiltinListClassSymbol = requiredClass("scalus.builtin.List")
    private val PlatformSpecificClassSymbol = requiredClass("scalus.builtin.PlatformSpecific")
    private val StringContextSymbol = requiredModule("scala.StringContext")
    private val Tuple2Symbol = requiredClass("scala.Tuple2")
    private val NothingSymbol = requiredClass("scala.Nothing")
    private val ByteStringModuleSymbol = requiredModule("scalus.builtin.ByteString")
    private val ByteStringStringInterpolatorsMethodSymbol =
        ByteStringModuleSymbol.requiredMethod("StringInterpolators")
    private val pmCompiler = new PatternMatchingCompiler(this)

    extension (t: Type)
        def isPair: Boolean = t.typeConstructor.classSymbol == PairSymbol
        def isList: Boolean = t.typeConstructor.classSymbol == ScalusBuiltinListClassSymbol
        def isMethodType: Boolean = t.isInstanceOf[MethodType]

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
    extension (t: Tree) def isData = t.tpe <:< converter.DataClassSymbol.typeRef

    enum CompileDef:
        case Compiling
        case Compiled(binding: TopLevelBinding)

    private val globalDefs: mutable.LinkedHashMap[FullName, CompileDef] =
        mutable.LinkedHashMap.empty
    private val globalDataDecls: mutable.LinkedHashMap[FullName, DataDecl] =
        mutable.LinkedHashMap.empty
    private val moduleDefsCache: mutable.Map[String, mutable.LinkedHashMap[FullName, SIR]] =
        mutable.LinkedHashMap.empty.withDefaultValue(mutable.LinkedHashMap.empty)

    private val CompileAnnot = requiredClassRef("scalus.Compile").symbol.asClass
    private val IgnoreAnnot = requiredClassRef("scalus.Ignore").symbol.asClass

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
        val start = System.currentTimeMillis()
        val tpl = td.rhs.asInstanceOf[Template]
        val bindings = tpl.body.flatMap {
            case dd: DefDef
                if !dd.symbol.flags.is(Flags.Synthetic)
                // uncomment to ignore derived methods
                // && !dd.symbol.name.startsWith("derived")
                    && !dd.symbol.hasAnnotation(IgnoreAnnot) =>
                compileStmt(HashSet.empty, dd, isGlobalDef = true)
            case vd: ValDef
                if !vd.symbol.flags.isOneOf(Flags.Synthetic | Flags.Case)
                // uncomment to ignore derived methods
                // && !vd.symbol.name.startsWith("derived")
                    && !vd.symbol.hasAnnotation(IgnoreAnnot) =>
                // println(s"valdef: ${vd.symbol.fullName}")
                compileStmt(HashSet.empty, vd, isGlobalDef = true)
            case _ => None
        }
        val module = Module(SirVersion, bindings.map(b => Binding(b.fullName.name, b.body)))
        writeModule(module, td.symbol.fullName.toString())
        val time = System.currentTimeMillis() - start
        report.echo(
          s"compiled Scalus module ${td.name} definitions: ${bindings.map(_.name).mkString(", ")} in ${time}ms"
        )
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

    def getAdtInfoFromConstroctorType(constrTpe: Type): AdtTypeInfo = {
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
            if constrTpe.typeConstructor =:= Tuple2Symbol.typeRef
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

    def primaryConstructorParams(typeSymbol: Symbol): List[Symbol] = {
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
    private def isConstructorVal(symbol: Symbol, @unused tpe: Type): Boolean =
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
                findAndReadModuleOfSymbol(moduleName).flatMap { case m @ Module(version, defs) =>
                    // println(s"Loaded module ${moduleName}, defs: ${defs}")
                    val defsMap =
                        mutable.LinkedHashMap.from(defs.map(d => FullName(d.name) -> d.value))
                    moduleDefsCache.put(moduleName, defsMap)
                    findAndLinkDefinition(defsMap, fullName, srcPos)
                }
        defn match
            case Some(d) =>
                // println(s"Found definition of ${fullName.name}")
                SIR.Var(fullName.name)
            case None =>
                error(SymbolNotFound(fullName.name, srcPos), SIR.Error("Symbol not found"))
    }

    def error[A](error: CompilationError, defaultValue: A): A = {
        report.error(error.message, error.srcPos)
        defaultValue
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
                        SIR.ExternalVar(
                          e.symbol.owner.fullName.toString(),
                          e.symbol.fullName.toString()
                        )
                    case scalus.Mode.Link =>
                        if e.symbol.defTree == EmptyTree then
                            linkDefinition(
                              e.symbol.owner.fullName.toString(),
                              fullName,
                              e.srcPos
                            )
                        else
                            // println(s"compileIdentOrQualifiedSelect2: ${e.symbol} ${e.symbol.defTree}")
                            // remember the symbol to avoid infinite recursion
                            globalDefs.update(fullName, CompileDef.Compiling)
                            // println(s"Tree of ${e}: ${e.tpe} isList: ${e.isList}")
                            // debugInfo(s"Tree of ${e.symbol}: ${e.symbol.tree.show}\n${e.symbol.tree}")
                            val b = compileStmt(HashSet.empty, e.symbol.defTree, isGlobalDef = true)
                            // remove the symbol from the linked hash map so the order of the definitions is preserved
                            globalDefs.remove(fullName)
                            b match
                                case None =>
                                case Some(b) =>
                                    traverseAndLink(b.body, e.symbol.sourcePos)
                                    globalDefs.update(
                                      fullName,
                                      CompileDef.Compiled(
                                        TopLevelBinding(fullName, b.recursivity, b.body)
                                      )
                                    )
                            SIR.Var(e.symbol.fullName.toString())
    }

    private def compileValDef(env: Env, vd: ValDef): Option[B] = {
        val name = vd.name
        // vars are not supported
        if vd.symbol.flags.is(Flags.Mutable) then
            error(VarNotSupported(vd, vd.srcPos), None)
            None
        /*
            lazy vals are not supported
            but we use givens for To/FromData instances
            which are compiled as final lazy vals
            Those are supported. They have the Given flag.
            Thus, we need to ignore them here.
         */
        else if vd.symbol.flags.isAllOf(Flags.Lazy, Flags.Given) then
            error(LazyValNotSupported(vd, vd.srcPos), None)
            None
        // ignore @Ignore annotated statements
        else if vd.symbol.hasAnnotation(IgnoreAnnot) then None
        // ignore PlatformSpecific statements
        // NOTE: check ps.tpe is not Nothing, as Nothing is a subtype of everything
        else if vd.tpe <:< PlatformSpecificClassSymbol.typeRef && !(vd.tpe =:= NothingSymbol.typeRef)
        then
            // println(s"Ignore PlatformSpecific: ${vd.symbol.fullName}")
            None
        else
            val bodyExpr = compileExpr(env, vd.rhs)
            Some(B(name.show, vd.symbol, Recursivity.NonRec, bodyExpr))
    }

    private def compileDefDef(env: Env, dd: DefDef, isGlobalDef: Boolean): Option[B] = {
        // ignore inline defs and @Ignore annotated statements
        if dd.symbol.flags.is(Flags.Inline) || dd.symbol.hasAnnotation(IgnoreAnnot) then None
        // ignore PlatformSpecific statements
        // NOTE: check ps.tpe is not Nothing, as Nothing is a subtype of everything
        else
            val params = dd.paramss.flatten.collect({ case vd: ValDef => vd })
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

            Some(B(dd.name.show, dd.symbol, Recursivity.Rec, bodyExpr))
    }

    private def compileStmt(env: Env, stmt: Tree, isGlobalDef: Boolean = false): Option[B] = {
        // report.echo(s"compileStmt  ${stmt.show} in ${env}")
        stmt match
            case vd: ValDef => compileValDef(env, vd)
            case dd: DefDef => compileDefDef(env, dd, isGlobalDef)
            case x          => Some(B("_", NoSymbol, Recursivity.NonRec, compileExpr(env, x)))
    }

    private def compileBlock(env: Env, stmts: immutable.List[Tree], expr: Tree): SIR = {
        val exprs = ListBuffer.empty[B]
        val exprEnv = stmts.foldLeft(env) {
            case (env, _: Import)  => env // ignore local imports
            case (env, _: TypeDef) => env // ignore local type definitions
            case (env, stmt) =>
                compileStmt(env, stmt) match
                    case None => env
                    case Some(bind) =>
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

    private val compileConstant: PartialFunction[Tree, scalus.uplc.Constant] = {
        case l @ Literal(c: Constant) =>
            c.tag match
                case Constants.BooleanTag => scalus.uplc.Constant.Bool(c.booleanValue)
                case Constants.StringTag  => scalus.uplc.Constant.String(c.stringValue)
                case Constants.UnitTag    => scalus.uplc.Constant.Unit
                case _ => error(LiteralTypeNotSupported(c, l.srcPos), scalus.uplc.Constant.Unit)
        case t @ Apply(bigintApply, List(SkipInline(Literal(c))))
            if bigintApply.symbol == converter.BigIntSymbol.requiredMethod(
              "apply",
              List(defn.StringClass.typeRef)
            ) && c.tag == Constants.StringTag =>
            scalus.uplc.Constant.Integer(BigInt(c.stringValue))
        case t @ Apply(bigintApply, List(SkipInline(Literal(c))))
            if bigintApply.symbol == converter.BigIntSymbol.requiredMethod(
              "apply",
              List(defn.IntType)
            ) && c.tag == Constants.IntTag =>
            scalus.uplc.Constant.Integer(BigInt(c.intValue))

        case Apply(i, List(Literal(c)))
            if i.symbol == converter.BigIntSymbol.requiredMethod("int2bigInt") =>
            scalus.uplc.Constant.Integer(BigInt(c.intValue))
        case expr if expr.symbol == converter.ByteStringSymbol.requiredMethod("empty") =>
            scalus.uplc.Constant.ByteString(scalus.builtin.ByteString.empty)
        case Apply(expr, List(Literal(c)))
            if expr.symbol == converter.ByteStringSymbol.requiredMethod("fromHex") =>
            scalus.uplc.Constant.ByteString(scalus.builtin.ByteString.fromHex(c.stringValue))
        case Apply(expr, List(Literal(c)))
            if expr.symbol == converter.ByteStringSymbol.requiredMethod("fromString") =>
            scalus.uplc.Constant.ByteString(scalus.builtin.ByteString.fromString(c.stringValue))
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
            if ByteStringStringInterpolatorsMethodSymbol == stringInterpolators.symbol
                && stringContext.symbol == StringContextSymbol && hex == termName("hex") &&
                const.tag == Constants.StringTag =>
            scalus.uplc.Constant.ByteString(scalus.builtin.ByteString.fromHex(const.stringValue))
    }

    private def typeReprToDefaultUni(tpe: Type, list: Tree): DefaultUni =
        if tpe =:= converter.BigIntClassSymbol.typeRef then DefaultUni.Integer
        else if tpe =:= defn.StringClass.typeRef then DefaultUni.String
        else if tpe =:= defn.BooleanClass.typeRef then DefaultUni.Bool
        else if tpe =:= defn.UnitClass.typeRef then DefaultUni.Unit
        else if tpe =:= converter.DataClassSymbol.typeRef then DefaultUni.Data
        else if tpe =:= converter.ByteStringClassSymbol.typeRef then DefaultUni.ByteString
        else if tpe.isPair then
            val List(t1, t2) = tpe.dealias.argInfos
            DefaultUni.Pair(typeReprToDefaultUni(t1, list), typeReprToDefaultUni(t2, list))
        else if tpe.isList then
            val t1 = tpe.dealias.argInfos.head
            DefaultUni.List(typeReprToDefaultUni(t1, list))
        else error(NotBuiltinTypeInBuiltinListConstruction(tpe, list), DefaultUni.Unit)

    private def compileBigIntOps(env: Env, lhs: Tree, op: Name, rhs: Tree, optree: Tree): SIR =
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
                error(
                  UnsupportedBigIntOp(op.show, optree.srcPos),
                  SIR.Error("Unsupported BigInt operation")
                )

    private def compileMatch(tree: Match, env: Env): SIR = {
        pmCompiler.compileMatch(tree, env)
    }

    private def compileBuiltinPairMethods(env: Env, fun: Name, pair: Tree, tree: Tree): SIR =
        fun.show match
            case "fst" =>
                SIR.Apply(SIR.Builtin(DefaultFun.FstPair), compileExpr(env, pair))
            case "snd" =>
                SIR.Apply(SIR.Builtin(DefaultFun.SndPair), compileExpr(env, pair))
            case _ => error(UnsupportedPairFunction(fun.toString, tree.srcPos), SIR.Error(""))

    private def compileBuiltinPairConstructor(
        env: Env,
        a: Tree,
        b: Tree,
        tpe1: Tree,
        tpe2: Tree,
        tree: Tree
    ): SIR =
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
            error(
              PairConstructionError(
                a,
                tpe1,
                a.isLiteral,
                a.isData,
                b,
                tpe2,
                b.isLiteral,
                b.isData,
                tree.srcPos
              ),
              SIR.Error("")
            )

    private def compileBuiltinListMethods(env: Env, lst: Tree, fun: Name): SIR =
        fun.show match
            case "head" =>
                SIR.Apply(SIR.Builtin(DefaultFun.HeadList), compileExpr(env, lst))
            case "tail" =>
                SIR.Apply(SIR.Builtin(DefaultFun.TailList), compileExpr(env, lst))
            case "isEmpty" =>
                SIR.Apply(SIR.Builtin(DefaultFun.NullList), compileExpr(env, lst))
            case _ =>
                error(UnsupportedListFunction(fun.toString, lst.srcPos), SIR.Error(""))

    private def compileBuiltinListConstructor(
        env: Env,
        ex: Tree,
        list: Tree,
        tpe: Tree,
        tree: Tree
    ): SIR =
        val tpeE = typeReprToDefaultUni(tpe.tpe, list)
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
                error(UnsupportedListApplyInvocation(tree, tpe, tree.srcPos), SIR.Error(""))

    private def compileApply(env: Env, f: Tree, args: List[Tree]): SIR =
        val fE = compileExpr(env, f)
        val argsE = args.map(compileExpr(env, _))
        if argsE.isEmpty then SIR.Apply(fE, SIR.Const(scalus.uplc.Constant.Unit))
        else argsE.foldLeft(fE)((acc, arg) => SIR.Apply(acc, arg))

    private def compileThrowException(ex: Tree): SIR =
        val msg = ex match
            case Apply(Select(New(tpt), nme.CONSTRUCTOR), immutable.List(Literal(msg), _*))
                if tpt.tpe <:< defn.ExceptionClass.typeRef =>
                msg.stringValue
            case term => "error"
        SIR.Error(msg)

    def compileExpr(env: Env, tree: Tree)(using Context): SIR = {
        // println(s"compileExpr: ${tree.showIndented(2)}, env: $env")
        if compileConstant.isDefinedAt(tree) then
            val const = compileConstant(tree)
            SIR.Const(const)
        else compileExpr2(env, tree)
    }

    private def compileExpr2(env: Env, tree: Tree)(using Context): SIR = {
        tree match
            case If(cond, t, f) =>
                SIR.IfThenElse(compileExpr(env, cond), compileExpr(env, t), compileExpr(env, f))
            case m: Match => compileMatch(m, env)
            // throw new Exception("error msg")
            // Supports any exception type that uses first argument as message
            case Apply(Ident(nme.throw_), immutable.List(ex)) => compileThrowException(ex)
            /* Handle PlatformSpecific builtins
           Builtins.sha2_256(using PlatformSpecific)(msg: ByteString): ByteString
           So we just ignore this PlatformSpecific argument and compile the rest
             */
            case Apply(builtin, immutable.List(ps))
                // NOTE: check ps.tpe is not Nothing, as Nothing is a subtype of everything
                if ps.tpe <:< PlatformSpecificClassSymbol.typeRef && !(ps.tpe =:= NothingSymbol.typeRef) =>
                compileExpr(env, builtin)
            // Boolean
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
            case Apply(Select(lhs, op), List(rhs))
                if lhs.tpe.widen =:= defn.BooleanType && op == nme.EQ =>
                val lhsExpr = compileExpr(env, lhs)
                val rhsExpr = compileExpr(env, rhs)
                SIR.IfThenElse(
                  lhsExpr,
                  rhsExpr,
                  SIR.IfThenElse(
                    rhsExpr,
                    SIR.Const(scalus.uplc.Constant.Bool(false)),
                    SIR.Const(scalus.uplc.Constant.Bool(true))
                  )
                )
            case Apply(Select(lhs, op), List(rhs))
                if lhs.tpe.widen =:= defn.BooleanType && op == nme.NE =>
                val lhsExpr = compileExpr(env, lhs)
                val rhsExpr = compileExpr(env, rhs)
                SIR.IfThenElse(
                  lhsExpr,
                  SIR.IfThenElse(
                    rhsExpr,
                    SIR.Const(scalus.uplc.Constant.Bool(false)),
                    SIR.Const(scalus.uplc.Constant.Bool(true))
                  ),
                  rhsExpr
                )
            // ByteString equality
            case Apply(Select(lhs, op), List(rhs))
                if lhs.tpe.widen =:= converter.ByteStringClassSymbol.typeRef && (op == nme.EQ || op == nme.NE) =>
                if !(rhs.tpe.widen =:= converter.ByteStringClassSymbol.typeRef) then
                    report.error(
                      s"""Equality is only allowed between the same types but here we have ${lhs.tpe.widen.show} == ${rhs.tpe.widen.show}
                         |Make sure you compare values of the same type""".stripMargin
                    )
                    SIR.Error("Equality is only allowed between the same types")
                else
                    val lhsExpr = compileExpr(env, lhs)
                    val rhsExpr = compileExpr(env, rhs)
                    val eq = SIR.Apply(
                      SIR.Apply(SIR.Builtin(DefaultFun.EqualsByteString), lhsExpr),
                      rhsExpr
                    )
                    if op == nme.EQ then eq else SIR.Not(eq)
            // BUILTINS
            case bi: Select if builtinsHelper.builtinFun(bi.symbol).isDefined =>
                builtinsHelper.builtinFun(bi.symbol).get
            case bi: Ident if builtinsHelper.builtinFun(bi.symbol).isDefined =>
                builtinsHelper.builtinFun(bi.symbol).get
            // BigInt stuff
            case Apply(optree @ Select(lhs, op), List(rhs))
                if lhs.tpe.widen =:= converter.BigIntClassSymbol.typeRef =>
                compileBigIntOps(env, lhs, op, rhs, optree)
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
            case Select(lst, fun) if lst.isList => compileBuiltinListMethods(env, lst, fun)
            case tree @ TypeApply(Select(list, name), immutable.List(tpe))
                if name == termName("empty") && list.tpe =:= requiredModule(
                  "scalus.builtin.List"
                ).typeRef =>
                val tpeE = typeReprToDefaultUni(tpe.tpe, tree)
                SIR.Const(scalus.uplc.Constant.List(tpeE, Nil))
            case Apply(
                  TypeApply(Select(list, name), immutable.List(tpe)),
                  immutable.List(arg)
                ) if name == termName("::") && list.isList =>
                val argE = compileExpr(env, arg)
                SIR.Apply(
                  SIR.Apply(SIR.Builtin(DefaultFun.MkCons), argE),
                  compileExpr(env, list)
                )
            case tree @ Apply(
                  TypeApply(Select(list, nme.apply), immutable.List(tpe)),
                  immutable.List(ex)
                ) if list.tpe =:= requiredModule("scalus.builtin.List").typeRef =>
                compileBuiltinListConstructor(env, ex, list, tpe, tree)
            // Pair BUILTINS
            // PAIR
            case Select(pair, fun) if pair.isPair =>
                compileBuiltinPairMethods(env, fun, pair, tree)
            case Apply(
                  TypeApply(Select(pair, nme.apply), immutable.List(tpe1, tpe2)),
                  immutable.List(a, b)
                ) if pair.tpe =:= requiredModule("scalus.builtin.Pair").typeRef =>
                compileBuiltinPairConstructor(env, a, b, tpe1, tpe2, tree)
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
            // f.apply[A, B](arg) => Apply(f, arg)
            /* When we have something like this:
             * (f: [A] => List[A] => A, a: A) => f[Data](a)
             * f.tpe will be a MethodType
             */
            case Apply(applied @ TypeApply(fun @ Select(f, nme.apply), _), args)
                if defn.isFunctionType(f.tpe.widen) || applied.tpe.isMethodType =>
                compileApply(env, f, args)
            // f.apply(arg) => Apply(f, arg)
            case Apply(Select(f, nme.apply), args) if defn.isFunctionType(f.tpe.widen) =>
                compileApply(env, f, args)
            case Ident(a) =>
                if isConstructorVal(tree.symbol, tree.tpe) then
                    compileNewConstructor(env, tree.tpe, Nil)
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
                    val lam = primaryConstructorParams(ts).foldRight(SIR.Var(ident.show)) {
                        case (f, acc) =>
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
            case Apply(f, args) => compileApply(env, f, args)
            // (x: T) => body
            case Block(
                  immutable.List(dd @ DefDef(nme.ANON_FUN, _, _, _)),
                  Closure(_, Ident(nme.ANON_FUN), _)
                ) =>
                compileStmt(env, dd).get.body
            case Block(stmt, expr) => compileBlock(env, stmt, expr)
            case Typed(expr, _)    => compileExpr(env, expr)
            case Inlined(_, bindings, expr) =>
                val r = compileBlock(env, bindings, expr)
                // val t = r.asTerm.show
                // report.info(s"Inlined: ${bindings}, ${expr.show}\n${t}", Position(SourceFile.current, globalPosition, 0))
                r
            case Return(expr, _) =>
                error(ReturnNotSupported(expr, tree.srcPos), SIR.Error("Return not supported"))
            case Assign(lhs, rhs) =>
                error(
                  ExpressionNotSupported("Variable assignment", tree.srcPos),
                  SIR.Error("Unsupported assign expression")
                )
            case Try(_, _, _) =>
                error(
                  ExpressionNotSupported("'try-catch-finally'", tree.srcPos),
                  SIR.Error("Unsupported try expression")
                )
            case WhileDo(cond, body) =>
                error(
                  ExpressionNotSupported("'while' expression", tree.srcPos),
                  SIR.Error("Unsupported while expression")
                )
            case x =>
                error(
                  ExpressionNotSupported(x.show, tree.srcPos),
                  SIR.Error("Unsupported expression")
                )
    }

    def compileToSIR(tree: Tree)(using Context): SIR = {
        // println(s"compileToSIR: ${tree}")
        val result = compileExpr(HashSet.empty, tree)
        val full = globalDefs.values.foldRight(result) {
            case (CompileDef.Compiled(b), acc) =>
                SIR.Let(b.recursivity, List(Binding(b.fullName.name, b.body)), acc)
            case (d, acc) =>
                error(
                  GenericError(
                    s"""Unexpected globalDefs state: $d
                  |$globalDefs
                  |It's likely a Scalus bug. Please, report it via GitHub Issues or Discord
                  |""".stripMargin,
                    tree.srcPos
                  ),
                  SIR.Error("")
                )

        }
        val dataDecls = globalDataDecls.foldRight(full) { case ((_, decl), acc) =>
            SIR.Decl(decl, acc)
        }
        dataDecls
    }

}
