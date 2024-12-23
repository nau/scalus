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
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.util.SrcPos
import dotty.tools.io.ClassPath
import scalus.flat.DecoderState
import scalus.flat.EncoderState
import scalus.flat.Flat
import scalus.flat.FlatInstantces.given
import scalus.sir.Binding
import scalus.sir.DataDecl
import scalus.sir.Module
import scalus.sir.Recursivity
import scalus.sir.SIR
import scalus.sir.SIRType
import scalus.sir.SIRVarStorage
import scalus.sir.SIRBuiltins
import scalus.sir.TypeBinding
import scalus.uplc.DefaultUni

import java.net.URL
import scala.collection.immutable
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import scala.annotation.unused
import scala.util.control.NonFatal
import scalus.builtin.ByteString

case class FullName(name: String)
object FullName:
    def apply(sym: Symbol)(using Context): FullName = FullName(sym.fullName.toString())

case class TopLevelBinding(fullName: FullName, recursivity: Recursivity, body: SIR)

case class B(name: String, symbol: Symbol, recursivity: Recursivity, body: SIR):
    def fullName(using Context) = FullName(symbol)

case class AdtTypeInfo(
    dataTypeSymbol: Symbol,
    dataTypeParams: List[Type],
    childrenSymbols: List[Symbol]
)

//sealed trait AdtTypeInfo
//
//case class AdtTypeInfoChildrenRecord(
//    childrenTypeSymbol: Symbol,
//    childrenTypeParams: List[Type],
//    data
//                            )

//case class AdtHierarchyTypeInfo(
//    dataTypeSymbol: Symbol,
//    dataTypeParams: List[Type],
//    childrenSymbols: List[Symbol]
//) extends AdtTypeInfo

case class AdtConstructorCallInfo(
    constructorTypeSymbol: Symbol,

    /** Type information of the base data type.
      */
    dataInfo: AdtTypeInfo,

    /** Type parameters of this type.
      */
    typeParams: List[Type]
)

final class SIRCompiler(mode: scalus.Mode)(using ctx: Context) {
    import tpd.*
    import SIRCompiler.Env

    val SirVersion = (0, 0)

    private val converter = new SIRConverter
    private val builtinsHelper = new BuiltinHelper
    private val PairSymbol = requiredClass("scalus.builtin.Pair")
    private val ScalusBuiltinListClassSymbol = requiredClass("scalus.builtin.List")
    private val PlatformSpecificClassSymbol = requiredClass("scalus.builtin.PlatformSpecific")
    private val StringContextSymbol = requiredModule("scala.StringContext")
    private val StringContextApplySymbol = StringContextSymbol.requiredMethod("apply")
    private val Tuple2Symbol = requiredClass("scala.Tuple2")
    private val NothingSymbol = requiredClass("scala.Nothing")
    private val ByteStringModuleSymbol = converter.ByteStringSymbol
    private val ByteStringSymbolHex = ByteStringModuleSymbol.requiredMethod("hex")
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
                compileStmt(Env.empty, dd, isGlobalDef = true) match
                    case CompileMemberDefResult.Compiled(b) => Some(b)
                    case _                                  => None
            case vd: ValDef
                if !vd.symbol.flags.isOneOf(Flags.Synthetic | Flags.Case)
                // uncomment to ignore derived methods
                // && !vd.symbol.name.startsWith("derived")
                    && !vd.symbol.hasAnnotation(IgnoreAnnot) =>
                // println(s"valdef: ${vd.symbol.fullName}")
                compileStmt(Env.empty, vd, isGlobalDef = true) match
                    case CompileMemberDefResult.Compiled(b) => Some(b)
                    case _                                  => None
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
        val bitSize = fl.bitSize(module)
        val enc = EncoderState(bitSize / 8 + 1)
        fl.encode(module, enc)
        try {
            enc.filler()
        } catch {
            case ex: ArrayIndexOutOfBoundsException =>
                println("Catched ArrayIndexOutOfBoundsException during encoding in filler()")
                println(
                  s"module: ${module.defs(0).name}, number of defs: ${module.defs.size}, bitSize: ${bitSize}"
                )

                val hs0 = scalus.utils.HashConsed.State.empty
                val hsc1 = scalus.utils.HashConsedEncoderState.withSize(1000)
                for b <- module.defs do {
                    println(s"def: ${b}")
                    // val bindingBitSize = scalus.flat.FlatInstantces.BindingFlat.bitSizeHC(b,hs0)
                }

                throw ex
        }
        output.write(enc.buffer)
        output.close()
    }

    def getAdtTypeInfo(dataType: Type): AdtTypeInfo = {
        val dataTypeParams = dataType match
            case AppliedType(tp, params) => params
            case _                       => Nil
        val dataTypeSymbol = dataType.typeSymbol
        val constructorSymbols = dataTypeSymbol.children
        AdtTypeInfo(dataTypeSymbol, dataTypeParams, constructorSymbols)
    }

    def getAdtConstructorCallInfo(constrTpe: Type): AdtConstructorCallInfo = {
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
        val optAdtBaseTypeSymbol = constrTpe.baseClasses.find(b =>
            // println(s"base class: ${b.show} ${b.flags.flagsString}")
            // TODO:  recheck.  Why ! trait ?
            b.flags.isAllOf(Flags.Sealed | Flags.Abstract) && !b.flags.is(Flags.Trait)
        )

        val typeArgs = constrTpe match
            case AppliedType(_, args) => args
            case _                    => Nil

        val info =
            if constrTpe.typeConstructor =:= Tuple2Symbol.typeRef
            then
                val typeArgs = constrTpe match
                    case AppliedType(_, args) => args
                    case _                    => Nil
                AdtConstructorCallInfo(
                  typeSymbol,
                  AdtTypeInfo(typeSymbol, typeArgs, List(typeSymbol)),
                  typeArgs
                )
            else
                optAdtBaseTypeSymbol match
                    case None => // case 1 or 2
                        val typeInfo = AdtTypeInfo(typeSymbol, typeArgs, List(typeSymbol))
                        AdtConstructorCallInfo(typeSymbol, typeInfo, typeArgs)
                    case Some(baseClassSymbol) =>
                        val adtBaseType = constrTpe.baseType(baseClassSymbol)
                        val baseDataParams = adtBaseType match
                            case AppliedType(_, args) => args
                            case _                    => Nil
                        if constrTpe.isSingleton then // case 3, 5
                            val typeInfo = AdtTypeInfo(
                              baseClassSymbol,
                              baseDataParams,
                              baseClassSymbol.children
                            )
                            AdtConstructorCallInfo(constrTpe.termSymbol, typeInfo, typeArgs)
                        else // case 4, 6
                            val typeInfo = AdtTypeInfo(
                              baseClassSymbol,
                              baseDataParams,
                              baseClassSymbol.children
                            )
                            AdtConstructorCallInfo(typeSymbol, typeInfo, typeArgs)
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

    def primaryConstructorTypeParams(typeSymbol: Symbol): List[Symbol] = {
        val fields = typeSymbol.primaryConstructor.paramSymss.flatten.filter(s => s.isType)
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
        args: immutable.List[Tree],
        srcPos: SrcPos
    ): SIR = {

        // val typeSymbol = tpe.typeSymbol
        // debugInfo(s"compileNewConstructor0")
        /* report.echo(
      s"compileNewConstructor1 ${typeSymbol} singleton ${tpe.isSingleton} companion: ${typeSymbol.maybeOwner.companionClass} " +
      s"${typeSymbol.children} widen: ${tpe.widen.typeSymbol}, widen.children: ${tpe.widen.typeSymbol.children} ${typeSymbol.maybeOwner.companionClass.children}"
      ) */

        val adtCallInfo = getAdtConstructorCallInfo(tpe)
        // report.echo(s"compileNewConstructor1 ${tpe.show} base type: ${adtInfo}")

        val argsE = args.map(compileExpr(env, _))
        val constrName = adtCallInfo.constructorTypeSymbol.name.show
        // sort by name to get a stable order
        val sortedConstructors = adtCallInfo.dataInfo.childrenSymbols.sortBy(_.name.show)

        val dataTypeSymbol = adtCallInfo.dataInfo.dataTypeSymbol
        val dataName = dataTypeSymbol.name.show
        // debugInfo(s"compileNewConstructor2: dataTypeSymbol $dataTypeSymbol, dataName $dataName, constrName $constrName, children ${constructors}")
        val dataDecl = globalDataDecls.get(FullName(dataTypeSymbol)) match
            case Some(decl) => decl
            case None =>
                val dataTypeParams = adtCallInfo.dataInfo.dataTypeParams.map { tp =>
                    SIRType.TypeVar(tp.typeSymbol.name.show)
                }
                val constrDecls = sortedConstructors.map { sym =>
                    val typeParams =
                        sym.typeParams.map(tp => SIRType.TypeVar(tp.name.show, Some(tp.hashCode)))
                    val envTypeVars1 = sym.typeParams.foldLeft(env.typeVars) { case (acc, tp) =>
                        acc + (tp -> SIRType.TypeVar(tp.name.show, Some(tp.hashCode)))
                    }
                    val envTypeVars2 = primaryConstructorTypeParams(sym).foldLeft(envTypeVars1) {
                        case (acc, tp) =>
                            acc + (tp -> SIRType.TypeVar(tp.name.show, Some(tp.hashCode)))
                    }
                    val nEnv = env.copy(typeVars = envTypeVars2)
                    val params = primaryConstructorParams(sym).map { p =>
                        val pType =
                            try sirTypeInEnv(p.info, srcPos, nEnv)
                            catch
                                case NonFatal(e) =>
                                    println(
                                      s"Error in sirTypeInEnv: ${p.info.show} ${p.info.widen.show}"
                                    )
                                    println(
                                      s"PrimaryConstructorParams: ${primaryConstructorParams(sym)}"
                                    )
                                    println(
                                      s"PrimaryConstructorTypeParams: ${primaryConstructorTypeParams(sym)}"
                                    )
                                    throw e
                        TypeBinding(p.name.show, pType)
                    }
                    val optBaseClass = sym.info.baseClasses.find { b =>
                        b.flags.is(Flags.Sealed) && b.children.contains(sym)
                    }
                    val baseTypeArgs = optBaseClass
                        .flatMap { bs =>
                            sym.info.baseType(bs) match
                                case AppliedType(_, args) =>
                                    Some(args.map(a => sirTypeInEnv(a, srcPos, nEnv)))
                                case _ => None
                        }
                        .getOrElse(Nil)
                    // TODO: add substoitution for parent type params
                    // scalus.sir.ConstrDecl(sym.name.show, SIRVarStorage.DEFAULT, params, typeParams, baseTypeArgs)
                    scalus.sir.ConstrDecl(sym.name.show, SIRVarStorage.DEFAULT, params, typeParams)
                }
                val decl = scalus.sir.DataDecl(dataName, constrDecls, dataTypeParams)
                globalDataDecls.addOne(FullName(dataTypeSymbol) -> decl)
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
        case SIR.ExternalVar(moduleName, name, tp) if !globalDefs.contains(FullName(name)) =>
            linkDefinition(moduleName, FullName(name), srcPos)
        case SIR.Let(recursivity, bindings, body) =>
            bindings.foreach(b => traverseAndLink(b.value, srcPos))
            traverseAndLink(body, srcPos)
        case SIR.LamAbs(name, term) => traverseAndLink(term, srcPos)
        case SIR.Apply(f, arg, tp) =>
            traverseAndLink(f, srcPos)
            traverseAndLink(arg, srcPos)
        case SIR.And(lhs, rhs) =>
            traverseAndLink(lhs, srcPos)
            traverseAndLink(rhs, srcPos)
        case SIR.Or(lhs, rhs) =>
            traverseAndLink(lhs, srcPos)
            traverseAndLink(rhs, srcPos)
        case SIR.Not(term) => traverseAndLink(term, srcPos)
        case SIR.IfThenElse(cond, t, f, tp) =>
            traverseAndLink(cond, srcPos)
            traverseAndLink(t, srcPos)
            traverseAndLink(f, srcPos)
        case SIR.Decl(data, term) => traverseAndLink(term, srcPos)
        case SIR.Constr(name, data, args) =>
            try
                globalDataDecls.put(FullName(data.name), data)
                args.foreach(a => traverseAndLink(a, srcPos))
            catch
                case NonFatal(e) =>
                    println(s"Error in traverseAndLink: ${e.getMessage}")
                    println(s"SIR= ${sir}")
                    throw e
        case SIR.Match(scrutinee, cases, rhsType) =>
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
                SIR.Var(fullName.name, d.tp)
            case None =>
                error(SymbolNotFound(fullName.name, srcPos), SIR.Error("Symbol not found"))
    }

    def error[A](error: CompilationError, defaultValue: A): A = {
        report.error(error.message, error.srcPos)
        if true then {
            throw new RuntimeException(error.message)
            // Thread.dumpStack()
        }
        defaultValue
    }

    private def compileIdentOrQualifiedSelect(env: Env, e: Tree): SIR = {
        val name = e.symbol.name.show
        val fullName = FullName(e.symbol)
        val isInLocalEnv = env.vars.contains(name)
        val isInGlobalEnv = globalDefs.contains(fullName)
        // println( s"compileIdentOrQualifiedSelect1: ${e.symbol} $name $fullName, term: ${e.show}, loc/glob: $isInLocalEnv/$isInGlobalEnv, env: ${env}" )
        (isInLocalEnv, isInGlobalEnv) match
            // global def, self reference, use the name
            case (true, true) =>
                val localType = env.vars(name)
                globalDefs(fullName) match
                    case CompileDef.Compiled(TopLevelBinding(_, _, body)) =>
                        val globalType = body.tp
                        if globalType != localType then
                            error(
                              TypeMismatch(
                                e.symbol.fullName.toString(),
                                localType,
                                globalType,
                                e.srcPos
                              ),
                              SIR.Var(e.symbol.fullName.toString(), localType)
                            )
                    case _ =>
                SIR.Var(e.symbol.fullName.toString(), localType)
            // local def, use the name
            case (true, false) =>
                SIR.Var(e.symbol.name.show, env.vars(name))
            // global def, use full name
            case (false, true) =>
                SIR.Var(e.symbol.fullName.toString(), sirTypeInEnv(e.tpe.widen, e.srcPos, env))
            case (false, false) =>
                mode match
                    case scalus.Mode.Compile =>
                        // println( s"external var: module ${e.symbol.owner.fullName.toString()}, ${e.symbol.fullName.toString()}" )
                        val valType = sirTypeInEnv(e.tpe.widen, e.srcPos, env)
                        try
                            SIR.ExternalVar(
                              e.symbol.owner.fullName.toString(),
                              e.symbol.fullName.toString(),
                              valType
                            )
                        catch
                            case NonFatal(ex) =>
                                println(s"Error in compileIdentOrQualifiedSelect: ${ex.getMessage}")
                                println(s"ExternalVar: ${e.symbol.fullName}")
                                println(s"tree: ${e.show}")
                                throw ex
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
                            val b = compileStmt(Env.empty, e.symbol.defTree, isGlobalDef = true)
                            // remove the symbol from the linked hash map so the order of the definitions is preserved
                            globalDefs.remove(fullName)
                            val tp = b match
                                case CompileMemberDefResult.Compiled(b) =>
                                    traverseAndLink(b.body, e.symbol.sourcePos)
                                    globalDefs.update(
                                      fullName,
                                      CompileDef.Compiled(
                                        TopLevelBinding(fullName, b.recursivity, b.body)
                                      )
                                    )
                                    b.body.tp
                                case CompileMemberDefResult.Ignored(tp) =>
                                    tp
                                case CompileMemberDefResult.Builtin(name, tp) =>
                                    tp
                                case _ =>
                                    sirTypeInEnv(e.tpe.widen, e.srcPos, env)
                            SIR.Var(e.symbol.fullName.toString(), tp)
    }

    enum CompileMemberDefResult {
        case Compiled(b: B)
        case Builtin(name: String, tp: SIRType)
        case Ignored(tp: SIRType)
        case NotSupported
    }

    private def compileValDef(env: Env, vd: ValDef): CompileMemberDefResult = {
        val name = vd.name
        // vars are not supported
        if vd.symbol.flags.is(Flags.Mutable) then
            error(VarNotSupported(vd, vd.srcPos), None)
            CompileMemberDefResult.NotSupported
        /*
            lazy vals are not supported
            but we use givens for To/FromData instances
            which are compiled as final lazy vals
            Those are supported. They have the Given flag.
            Thus, we need to ignore them here.
         */
        else if vd.symbol.flags.isAllOf(Flags.Lazy, Flags.Given) then
            error(LazyValNotSupported(vd, vd.srcPos), None)
            CompileMemberDefResult.NotSupported
        // ignore @Ignore annotated statements
        else if vd.symbol.hasAnnotation(IgnoreAnnot) then
            CompileMemberDefResult.Ignored(sirTypeInEnv(vd.tpe, vd.srcPos, env))
        // ignore PlatformSpecific statements
        // NOTE: check ps.tpe is not Nothing, as Nothing is a subtype of everything
        else if vd.tpe <:< PlatformSpecificClassSymbol.typeRef && !(vd.tpe =:= NothingSymbol.typeRef)
        then CompileMemberDefResult.Builtin(name.show, SIRType.FreeUnificator)
        else
            // TODO store comments in the SIR
            // vd.rawComment
            val bodyExpr = compileExpr(env, vd.rhs)
            CompileMemberDefResult.Compiled(B(name.show, vd.symbol, Recursivity.NonRec, bodyExpr))
    }

    private def compileDefDef(
        env: Env,
        dd: DefDef,
        isGlobalDef: Boolean
    ): CompileMemberDefResult = {
        // ignore inline defs and @Ignore annotated statements
        if dd.symbol.flags.is(Flags.Inline) || dd.symbol.hasAnnotation(IgnoreAnnot) then
            CompileMemberDefResult.Ignored(sirTypeInEnv(dd.tpe, dd.srcPos, env))
            // ignore PlatformSpecific statements
            // NOTE: check ps.tpe is not Nothing, as Nothing is a subtype of everything
        else
            // TODO store comments in the SIR
            // dd.rawComment
            val params = dd.paramss.flatten.collect({ case vd: ValDef => vd })
            val typeParams = dd.paramss.flatten.collect({ case td: TypeDef => td })
            val typeParamsMap = typeParams.foldLeft(Map.empty[Symbol, SIRType]) { case (acc, td) =>
                acc + (td.symbol -> SIRType.TypeVar(td.symbol.name.show, Some(td.symbol.hashCode)))
            }
            val paramNameTypes =
                if params.isEmpty then
                    List(("_" -> SIRType.VoidPrimitive)) /* Param for () argument */
                else
                    params.map { case v: ValDef =>
                        val tEnv =
                            SIRTypesHelper.SIRTypeEnv(v.srcPos, env.typeVars ++ typeParamsMap)
                        val vType =
                            try sirTypeInEnv(v.tpe, tEnv)
                            catch
                                case NonFatal(e) =>
                                    println(
                                      s"Error in sirTypeInEnv: ${v.tpe.show} ${v.tpe.widen.show}"
                                    )
                                    println(s"Params: ${params}")
                                    println(s"TypeParams: ${typeParams}")
                                    throw e
                        (v.symbol.name.show, vType)
                    }
            val body = dd.rhs
            val selfName = if isGlobalDef then FullName(dd.symbol).name else dd.symbol.name.show
            val selfType = sirTypeInEnv(dd.tpe, SIRTypesHelper.SIRTypeEnv(dd.srcPos, env.typeVars))
            val nTypeVars = env.typeVars ++ typeParamsMap
            val nVars = env.vars ++ paramNameTypes + (selfName -> selfType)
            val bE = compileExpr(env.copy(vars = nVars, typeVars = nTypeVars), body)
            val bodyExpr: scalus.sir.SIR =
                paramNameTypes.foldRight(bE) { (nameType, acc) =>
                    SIR.LamAbs(SIR.Var(nameType._1, nameType._2), acc)
                }
            CompileMemberDefResult.Compiled(B(dd.name.show, dd.symbol, Recursivity.Rec, bodyExpr))
    }

    private def compileStmt(
        env: Env,
        stmt: Tree,
        isGlobalDef: Boolean = false
    ): CompileMemberDefResult = {
        // report.echo(s"compileStmt  ${stmt.show} in ${env}")
        stmt match
            case vd: ValDef => compileValDef(env, vd)
            case dd: DefDef =>
                compileDefDef(env, dd, isGlobalDef)
            case x =>
                CompileMemberDefResult.Compiled(
                  B(
                    s"__${stmt.source.file.name.takeWhile(_.isLetterOrDigit)}_line_${stmt.srcPos.line}",
                    NoSymbol,
                    Recursivity.NonRec,
                    compileExpr(env, x)
                  )
                )
    }

    private def compileBlock(env: Env, stmts: immutable.List[Tree], expr: Tree): SIR = {
        if (env.debug) then println(s"compileBlock: ${stmts.map(_.show).mkString("\n")}")
        val exprs = ListBuffer.empty[B]
        val exprEnv = stmts.foldLeft(env) {
            case (env, _: Import)  => env // ignore local imports
            case (env, _: TypeDef) => env // ignore local type definitions
            case (env, stmt) =>
                compileStmt(env, stmt) match
                    case CompileMemberDefResult.Compiled(bind) =>
                        exprs += bind
                        env + (bind.name -> bind.body.tp)
                    case _ => env

        }
        val exprExpr = compileExpr(exprEnv, expr)
        if (env.debug) then
            println(s"compileBlock: expr=${expr.show}")
            println(s"compileBlock: exprExprs.tp=${exprExpr.tp.show}")
        val retval = exprs.foldRight(exprExpr) { (bind, expr) =>
            SIR.Let(bind.recursivity, List(Binding(bind.name, bind.body)), expr)
        }
        if (env.debug) then println(s"compileBlock: retval.tp=${retval.tp.show}")
        retval
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
        // hex"deadbeef" as ByteString using Scala 3 StringContext extension
        case Apply(
              Apply(
                byteStringHex,
                List(Apply(stringContextApply, List(SeqLiteral(List(Literal(c)), _))))
              ),
              List(SeqLiteral(List(), _))
            )
            if byteStringHex.symbol == ByteStringSymbolHex
                && stringContextApply.symbol == StringContextApplySymbol =>
            scalus.uplc.Constant.ByteString(scalus.builtin.ByteString.fromHex(c.stringValue))
        // hex"deadbeef" as ByteString for Scala 2 implicit StringInterpolators
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
                  SIR.Apply(
                    SIRBuiltins.addInteger,
                    compileExpr(env, lhs),
                    SIRType.IntegerPrimitive ->: SIRType.IntegerPrimitive
                  ),
                  compileExpr(env, rhs),
                  SIRType.IntegerPrimitive
                )
            case nme.MINUS =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.subtractInteger,
                    compileExpr(env, lhs),
                    SIRType.IntegerPrimitive ->: SIRType.IntegerPrimitive
                  ),
                  compileExpr(env, rhs),
                  SIRType.IntegerPrimitive
                )
            case nme.MUL =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.multiplyInteger,
                    compileExpr(env, lhs),
                    SIRType.IntegerPrimitive ->: SIRType.IntegerPrimitive
                  ),
                  compileExpr(env, rhs),
                  SIRType.IntegerPrimitive
                )
            case nme.DIV =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.divideInteger,
                    compileExpr(env, lhs),
                    SIRType.IntegerPrimitive ->: SIRType.IntegerPrimitive
                  ),
                  compileExpr(env, rhs),
                  SIRType.IntegerPrimitive
                )
            case nme.MOD =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.remainderInteger,
                    compileExpr(env, lhs),
                    SIRType.IntegerPrimitive ->: SIRType.IntegerPrimitive
                  ),
                  compileExpr(env, rhs),
                  SIRType.IntegerPrimitive
                )
            case nme.LT =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.lessThanInteger,
                    compileExpr(env, lhs),
                    SIRType.IntegerPrimitive ->: SIRType.BooleanPrimitive
                  ),
                  compileExpr(env, rhs),
                  SIRType.BooleanPrimitive
                )
            case nme.LE =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.lessThanEqualsInteger,
                    compileExpr(env, lhs),
                    SIRType.IntegerPrimitive ->: SIRType.BooleanPrimitive
                  ),
                  compileExpr(env, rhs),
                  SIRType.BooleanPrimitive
                )
            case nme.GT =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.lessThanInteger,
                    compileExpr(env, rhs),
                    SIRType.IntegerPrimitive ->: SIRType.BooleanPrimitive
                  ),
                  compileExpr(env, lhs),
                  SIRType.BooleanPrimitive
                )
            case nme.GE =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.lessThanEqualsInteger,
                    compileExpr(env, rhs),
                    SIRType.IntegerPrimitive ->: SIRType.BooleanPrimitive
                  ),
                  compileExpr(env, lhs),
                  SIRType.BooleanPrimitive
                )
            case nme.EQ =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.equalsInteger,
                    compileExpr(env, lhs),
                    SIRType.IntegerPrimitive ->: SIRType.BooleanPrimitive
                  ),
                  compileExpr(env, rhs),
                  SIRType.BooleanPrimitive
                )
            case nme.NE =>
                SIR.Not(
                  SIR.Apply(
                    SIR.Apply(
                      SIRBuiltins.equalsInteger,
                      compileExpr(env, lhs),
                      SIRType.IntegerPrimitive ->: SIRType.BooleanPrimitive
                    ),
                    compileExpr(env, rhs),
                    SIRType.BooleanPrimitive
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
                val expr = compileExpr(env, pair)
                expr.tp match
                    case SIRType.Pair(t1, t2) =>
                        SIR.Apply(SIRBuiltins.fstPair, expr, t1)
                    case other =>
                        error(
                          TypeMismatch(
                            fun.toString,
                            SIRType.Pair(SIRType.TypeVar("A"), SIRType.TypeVar("B")),
                            other,
                            tree.srcPos
                          ),
                          SIR.Error("")
                        )
            case "snd" =>
                val expr = compileExpr(env, pair)
                expr.tp match
                    case SIRType.Pair(t1, t2) =>
                        SIR.Apply(SIRBuiltins.sndPair, expr, t2)
                    case other =>
                        error(
                          TypeMismatch(
                            fun.toString,
                            SIRType.Pair(SIRType.TypeVar("A"), SIRType.TypeVar("B")),
                            other,
                            tree.srcPos
                          ),
                          SIR.Error("")
                        )
            case _ => error(UnsupportedPairFunction(fun.toString, tree.srcPos), SIR.Error(""))

    private def compileBuiltinPairConstructor(
        env: Env,
        a: Tree,
        b: Tree,
        tpe1: Tree,
        tpe2: Tree,
        tree: Tree
    ): SIR =
        if (env.debug) then
            println(
              s"compileBuiltinPairConstructor: ${a.show}, ${b.show}, tpe1: $tpe1, tpe2: $tpe2"
            )
        // We can create a Pair by either 2 literals as (con pair...)
        // or 2 Data variables using MkPairData builtin
        if a.isLiteral && b.isLiteral then
            SIR.Const(
              scalus.uplc.Constant.Pair(compileConstant(a), compileConstant(b)),
              SIRType.Pair(sirTypeInEnv(a.tpe, a.srcPos, env), sirTypeInEnv(b.tpe, b.srcPos, env))
            )
        else if a.isData && b.isData then
            val exprA = compileExpr(env, a)
            val exprB = compileExpr(env, b)
            // val typeB = SIRType.TypeVar("B")
            SIR.Apply(
              SIR.Apply(
                SIRBuiltins.mkPairData,
                exprA,
                SIRType.Fun(exprB.tp, SIRType.Pair(exprA.tp, exprB.tp))
              ),
              exprB,
              SIRType.Pair(exprA.tp, exprB.tp)
            )
        else
            //  TODO: implement generic mkPair ?
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

    private def compileBuiltinListMethods(env: Env, lst: Tree, fun: Name, targs: List[Tree]): SIR =
        if (env.debug) then
            println(s"compileBuiltinListMethods: ${lst.show}, fun: $fun, targs: $targs")
        fun.show match
            case "head" =>
                val exprA = compileExpr(env, lst)
                exprA.tp match
                    case SIRType.List(t) =>
                        SIR.Apply(SIRBuiltins.headList, exprA, t)
                    case other =>
                        println(s"expected that exprA.tp ${exprA} is List, but got: ${other}")
                        throw new Exception("expected that exprA.tp is List")
                        error(
                          TypeMismatch(
                            fun.toString,
                            SIRType.List(SIRType.TypeVar("A")),
                            other,
                            lst.srcPos
                          ),
                          SIR.Error("")
                        )
            case "tail" =>
                val exprArg = compileExpr(env, lst)
                SIR.Apply(SIRBuiltins.tailList, exprArg, exprArg.tp)
            case "isEmpty" =>
                SIR.Apply(SIRBuiltins.nullList, compileExpr(env, lst), SIRType.BooleanPrimitive)
            case _ =>
                error(UnsupportedListFunction(fun.toString, lst.srcPos), SIR.Error(""))

    private def compileBuiltinListConstructor(
        env: Env,
        ex: Tree,
        list: Tree,
        tpe: Tree,
        tree: Tree
    ): SIR =
        if (env.debug) then
            println(s"compileBuiltinListConstructor: ${ex.show}, list: $list, tpe: $tpe")
        val tpeE = typeReprToDefaultUni(tpe.tpe, list)
        val tpeTp = sirTypeInEnv(tpe.tpe, tree.srcPos, env)
        val listTp = SIRType.List(tpeTp)
        if (env.debug) then
            println(
              s"compileBuiltinListConstructor: tpeE: $tpeE, tpeTp: $tpeTp, listTp: $listTp, listTp.show=${listTp.show}"
            )
        ex match
            case SeqLiteral(args, _) =>
                val allLiterals = args.forall(arg => compileConstant.isDefinedAt(arg))
                if allLiterals then
                    val lits = args.map(compileConstant)
                    SIR.Const(scalus.uplc.Constant.List(tpeE, lits), listTp)
                else
                    val nil: SIR = SIR.Const(scalus.uplc.Constant.List(tpeE, Nil), SIRType.List.Nil)
                    val retval = args.foldRight(nil) { (arg, acc) =>
                        SIR.Apply(
                          SIR.Apply(
                            SIRBuiltins.mkCons,
                            compileExpr(env, arg),
                            SIRType.Fun(listTp, listTp)
                          ),
                          acc,
                          listTp
                        )
                    }
                    if (env.debug) then
                        println(s"compileBuiltinListConstructor: retval: $retval")
                        println(s"compileBuiltinListConstructor: retval.tp: ${retval.tp.show}")
                    retval
            case _ =>
                error(UnsupportedListApplyInvocation(tree, tpe, tree.srcPos), SIR.Error(""))

    private def compileApply(
        env0: Env,
        f: Tree,
        targs: List[Tree],
        args: List[Tree],
        applyTpe: Type,
        applyTree: Apply
    ): SIR = {
        if (env0.debug) then
            println(
              s"compileApply: ${f.show}, targs: $targs, args: $args, applyTpe: $applyTpe, applyTree: $applyTree"
            )
        val env = fillTypeParamInTypeApply(f.symbol, targs, env0)
        val fE = compileExpr(env, f)
        val applySirType = sirTypeInEnv(applyTpe, applyTree.srcPos, env)
        val argsE = args.map(compileExpr(env, _))
        if argsE.isEmpty then
            SIR.Apply(fE, SIR.Const(scalus.uplc.Constant.Unit, SIRType.VoidPrimitive), applySirType)
        else
            // (f : (arg1 -> args2 -> ... -> res))
            // Apply(f, arg1) arg2 -> ... -> res)
            //  ....
            // Apply(...Apply(... f, arg1), arg2),,, ) res)
            val partTpes = argsE.foldRight(applySirType)((a, acc) => SIRType.Fun(a.tp, acc))
            val (applyExpr, applyTpe) = argsE.foldLeft((fE, partTpes)) { (acc, arg) =>
                val (fun, tp) = acc
                val nTp = tp match
                    case SIRType.Fun(t1, t2) => t2
                    case _ =>
                        error(
                          TypeMismatch(
                            "Function type",
                            SIRType.Fun(SIRType.TypeVar("A"), SIRType.TypeVar("B")),
                            tp,
                            f.srcPos
                          ),
                          SIRType.VoidPrimitive
                        )
                (SIR.Apply(fun, arg, nTp), nTp)
            }
            applyExpr
    }

    private def compileThrowException(ex: Tree): SIR =
        val msg = ex match
            case Apply(Select(New(tpt), nme.CONSTRUCTOR), immutable.List(Literal(msg), _*))
                if tpt.tpe <:< defn.ExceptionClass.typeRef =>
                msg.stringValue
            case term => "error"
        SIR.Error(msg)

    def compileExpr[T](env: Env, tree: Tree)(using Context): SIR = {
        if (env.debug) then println(s"compileExpr: ${tree.showIndented(2)}, env: $env")
        if compileConstant.isDefinedAt(tree) then
            val const = compileConstant(tree)
            SIR.Const(const, sirTypeInEnv(tree.tpe, tree.srcPos, env))
        else compileExpr2(env, tree)
    }

    private def compileExpr2(env: Env, tree: Tree)(using Context): SIR = {
        tree match
            case If(cond, t, f) =>
                if (env.debug) then println(s"compileExpr2: If ${cond.show}, ${t.show}, ${f.show}")
                val nEnv = env.copy(level = env.level + 1)
                val ct = compileExpr(nEnv, t)
                val cf = compileExpr(nEnv, f)
                val sirTp = sirTypeInEnv(tree.tpe, tree.srcPos, env)
                SIR.IfThenElse(compileExpr(nEnv, cond), ct, cf, sirTp)
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
                    SIR.Const(scalus.uplc.Constant.Bool(false), SIRType.BooleanPrimitive),
                    SIR.Const(scalus.uplc.Constant.Bool(true), SIRType.BooleanPrimitive),
                    SIRType.BooleanPrimitive
                  ),
                  SIRType.BooleanPrimitive
                )
            case Apply(Select(lhs, op), List(rhs))
                if lhs.tpe.widen =:= defn.BooleanType && op == nme.NE =>
                val lhsExpr = compileExpr(env, lhs)
                val rhsExpr = compileExpr(env, rhs)
                SIR.IfThenElse(
                  lhsExpr,
                  SIR.IfThenElse(
                    rhsExpr,
                    SIR.Const(scalus.uplc.Constant.Bool(false), SIRType.BooleanPrimitive),
                    SIR.Const(scalus.uplc.Constant.Bool(true), SIRType.BooleanPrimitive),
                    SIRType.BooleanPrimitive
                  ),
                  rhsExpr,
                  SIRType.BooleanPrimitive
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
                      SIR.Apply(
                        SIRBuiltins.equalsByteString,
                        lhsExpr,
                        SIRType.ByteStringPrimitive ->: SIRType.BooleanPrimitive
                      ),
                      rhsExpr,
                      SIRType.BooleanPrimitive
                    )
                    if op == nme.EQ then eq else SIR.Not(eq)
            // String equality
            case Apply(Select(lhs, op), List(rhs))
                if lhs.tpe.widen =:= defn.StringClass.typeRef && (op == nme.EQ || op == nme.NE) =>
                val lhsExpr = compileExpr(env, lhs)
                val rhsExpr = compileExpr(env, rhs)
                val eq =
                    SIR.Apply(
                      SIR.Apply(
                        SIRBuiltins.equalsString,
                        lhsExpr,
                        SIRType.StringPrimitive ->: SIRType.BooleanPrimitive
                      ),
                      rhsExpr,
                      SIRType.BooleanPrimitive
                    )
                if op == nme.EQ then eq else SIR.Not(eq)
            // Data equality
            case Apply(Select(lhs, op), List(rhs))
                if lhs.tpe.widen <:< converter.DataClassSymbol.typeRef && (op == nme.EQ || op == nme.NE) =>
                val lhsExpr = compileExpr(env, lhs)
                val rhsExpr = compileExpr(env, rhs)
                val eq =
                    SIR.Apply(
                      SIR.Apply(
                        SIRBuiltins.equalsData,
                        lhsExpr,
                        SIRType.Fun(SIRType.Data, SIRType.BooleanPrimitive)
                      ),
                      rhsExpr,
                      SIRType.BooleanPrimitive
                    )
                if op == nme.EQ then eq else SIR.Not(eq)
            // BUILTINS
            case bi: Select if builtinsHelper.builtinFun(bi.symbol).isDefined =>
                if (env.debug) then println(s"compileExpr: builtinFun: ${bi.symbol}")
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
                    SIRBuiltins.subtractInteger,
                    SIR.Const(scalus.uplc.Constant.Integer(BigInt(0)), SIRType.IntegerPrimitive),
                    SIRType.IntegerPrimitive ->: SIRType.IntegerPrimitive
                  ),
                  compileExpr(env, expr),
                  SIRType.IntegerPrimitive
                )
            // List BUILTINS
            case TypeApply(Select(lst, fun), targs) if lst.isList =>
                compileBuiltinListMethods(env, lst, fun, targs)
            case Select(lst, fun) if lst.isList => compileBuiltinListMethods(env, lst, fun, Nil)
            case tree @ TypeApply(Select(list, name), immutable.List(tpe))
                if name == termName("empty") && list.tpe =:= requiredModule(
                  "scalus.builtin.List"
                ).typeRef =>
                val tpeE = typeReprToDefaultUni(tpe.tpe, tree)
                SIR.Const(
                  scalus.uplc.Constant.List(tpeE, Nil),
                  SIRType.List(sirTypeInEnv(tpe.tpe, tree.srcPos, env))
                )
            case Apply(
                  TypeApply(Select(list, name), immutable.List(tpe)),
                  immutable.List(arg)
                ) if name == termName("::") && list.isList =>
                val argE = compileExpr(env, arg)
                val exprType = sirTypeInEnv(tree.tpe, tree.srcPos, env)
                SIR.Apply(
                  SIR.Apply(SIRBuiltins.mkCons, argE, SIRType.Fun(exprType, exprType)),
                  compileExpr(env, list),
                  exprType
                )
            case tree @ Apply(
                  TypeApply(Select(list, nme.apply), immutable.List(ltpe)),
                  immutable.List(ex)
                ) if list.tpe =:= requiredModule("scalus.builtin.List").typeRef =>
                compileBuiltinListConstructor(env, ex, list, ltpe, tree)
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
                compileNewConstructor(env, f.tpe, args, tree)
            case Apply(con @ Select(f, nme.CONSTRUCTOR), args) =>
                compileNewConstructor(env, f.tpe, args, tree)
            // (a, b) as scala.Tuple2.apply(a, b)
            // we need to special-case it because we use scala-library 2.13.x
            // which does not include TASTy so we can't access the method body
            case Apply(TypeApply(app @ Select(f, nme.apply), _), args)
                if app.symbol.fullName.show == "scala.Tuple2$.apply" =>
                compileNewConstructor(env, tree.tpe, args, tree)
            case Apply(app @ Select(f, nme.apply), args)
                if app.symbol.fullName.show == "scala.Tuple2$.apply" =>
                compileNewConstructor(env, tree.tpe, args, tree)
            // f.apply[A, B](arg) => Apply(f, arg)
            /* When we have something like this:
             * (f: [A] => List[A] => A, a: A) => f[Data](a)
             * f.tpe will be a MethodType
             */
            case a @ Apply(applied @ TypeApply(fun @ Select(f, nme.apply), targs), args)
                if defn.isFunctionType(f.tpe.widen) || applied.tpe.isMethodType =>
                compileApply(env, f, targs, args, tree.tpe, a)
            // f.apply(arg) => Apply(f, arg)
            case a @ Apply(Select(f, nme.apply), args) if defn.isFunctionType(f.tpe.widen) =>
                compileApply(env, f, Nil, args, tree.tpe, a)
            case Ident(a) =>
                if isConstructorVal(tree.symbol, tree.tpe) then
                    compileNewConstructor(env, tree.tpe, Nil, tree)
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
                    val selType = sirTypeInEnv(sel.tpe.widen.dealias, sel, env)
                    SIR.Select(lhs, ident.show, selType)
                // else if obj.symbol.isPackageDef then
                // compileExpr(env, obj)
                else if isConstructorVal(tree.symbol, tree.tpe) then
                    compileNewConstructor(env, tree.tpe, Nil, tree.srcPos)
                else compileIdentOrQualifiedSelect(env, tree)
            // ignore asInstanceOf
            case TypeApply(Select(e, nme.asInstanceOf_), _) => compileExpr(env, e)
            // Ignore type application
            case TypeApply(f, targs) =>
                val nEnv = fillTypeParamInTypeApply(f.symbol, targs, env)
                compileExpr(nEnv, f)
            // Generic Apply
            case a @ Apply(pf @ TypeApply(f, targs), args) =>
                compileApply(env, f, targs, args, tree.tpe, a)
            case app @ Apply(f, args) =>
                compileApply(env, f, Nil, args, tree.tpe, app)
            // (x: T) => body
            case Block(
                  immutable.List(dd @ DefDef(nme.ANON_FUN, _, _, _)),
                  Closure(_, Ident(nme.ANON_FUN), _)
                ) =>
                compileStmt(env, dd) match
                    case CompileMemberDefResult.Compiled(b) => b.body
                    case CompileMemberDefResult.Ignored(tp) =>
                        error(
                          GenericError("Ignoring closure", tree.srcPos),
                          SIR.Error("Ignored closure")
                        )
                    case CompileMemberDefResult.Builtin(name, tp) =>
                        error(
                          GenericError("Builtin library can be part of user code", tree.srcPos),
                          SIR.Error("Builtin definition")
                        )
                    case _ =>
                        // assume,  that if we have here unsupported, error is already reported.
                        SIR.Error("Closure with not supported form")
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

    private def fillTypeParamInTypeApply(sym: Symbol, targs: List[Tree], env: Env): Env = {
        val tparams = sym.typeParams
        val targsSirTypes = targs.map(t =>
            val wt = t.tpe.widen
            sirTypeInEnv(wt, t.srcPos, env)
        )
        val nTypeVars = tparams
            .zip(targsSirTypes)
            .map { case (tp, v) =>
                tp -> v
            }
            .toMap
        env.copy(typeVars = env.typeVars ++ nTypeVars)
    }

    def compileToSIR(tree: Tree, debug: Boolean)(using Context): SIR = {
        val result = compileExpr(Env.empty.copy(debug = debug), tree)
        val full: SIR = globalDefs.values.foldRight(result) {
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
        val dataDecls = globalDataDecls.foldRight((full: SIR)) { case ((_, decl), acc) =>
            SIR.Decl(decl, acc)
        }
        dataDecls
    }

    // def sirType(tp: Type, srcPos: SrcPos): SIRType = {
    //        sirTypeInEnv(tp, SIRTypesHelper.SIRTypeEnv(srcPos, Map.empty))
    // }

    def sirTypeInEnv(tp: Type, srcPos: SrcPos, env: Env): SIRType = {
        sirTypeInEnv(tp, SIRTypesHelper.SIRTypeEnv(srcPos, env.typeVars))
    }

    protected def sirTypeInEnv(tp: Type, env: SIRTypesHelper.SIRTypeEnv): SIRType = {
        try SIRTypesHelper.sirTypeInEnv(tp, env)
        catch
            case e: SIRTypesHelper.TypingException =>
                println(s"Error wjile typing: ${tp.show}: ${e.msg},")
                println(s"env.vars=${env.vars}");
                if true then {
                    throw e
                }
                val retval = error(
                  GenericError(e.msg, e.pos),
                  SIRType.TypeError(s"tp: ${tp.show}, ${e.msg}", e)
                )
                retval
    }

}

object SIRCompiler {

    case class Env(
        vars: Map[String, SIRType],
        typeVars: Map[Symbol, SIRType],
        debug: Boolean = false,
        level: Int = 0
    ) {

        def ++(bindings: Iterable[(String, SIRType)]): Env = copy(vars = vars ++ bindings)

        def +(ntpe: (String, SIRType)): Env = copy(vars = vars + ntpe)

        def withDebug: Env = copy(debug = true)

        def withoutDebug: Env = copy(debug = false)

    }

    object Env {

        def empty = Env(Map.empty, Map.empty)

    }

}
