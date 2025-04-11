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
import dotty.tools.dotc.util.{NoSourcePosition, SourcePosition, SrcPos}
import scalus.flat.EncoderState
import scalus.flat.Flat
import scalus.flat.FlatInstantces.given
import scalus.sir.AnnotationsDecl
import scalus.sir.Binding
import scalus.sir.ConstrDecl
import scalus.sir.DataDecl
import scalus.sir.Module
import scalus.sir.Recursivity
import scalus.sir.SIR
import scalus.sir.SIRType
import scalus.sir.SIRVarStorage
import scalus.sir.SIRBuiltins
import scalus.sir.SIRPosition
import scalus.sir.SIRVarStorage.LocalUPLC
import scalus.sir.TypeBinding
import scalus.uplc.DefaultUni

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import scala.annotation.unused
import scala.util.control.NonFatal

case class FullName(name: String)
object FullName:
    def apply(sym: Symbol)(using Context): FullName = FullName(sym.fullName.toString)

/** @param dataTypeSymbol
  *   \- the symbol of the base data type
  * @param dataTypeParams
  *   \- type parameters of the base data type
  * @param constructorsSymbols
  *   \- constructors symbols in the order of definition
  * @param parentSymbol
  *   \- the parent symbol of the base data type, if it exists
  */
case class AdtTypeInfo(
    dataTypeSymbol: Symbol,
    dataTypeParams: List[Type],
    constructorsSymbols: List[Symbol],
    parentSymbol: Option[Symbol]
)

/** Information about a constructor call.
  * @param name
  *   Name of the constructor.
  * @param dataInfo
  *   Type information of the base data type.
  */
case class AdtConstructorCallInfo(
    shortName: String,
    fullName: String,
    dataInfo: AdtTypeInfo
)

object AdtConstructorCallInfo {
    def apply(constructorTypeSymbol: Symbol, dataInfo: AdtTypeInfo)(using
        Context
    ): AdtConstructorCallInfo =
        AdtConstructorCallInfo(
          constructorTypeSymbol.name.show,
          constructorTypeSymbol.fullName.show,
          dataInfo
        )
}

case class TopLevelBinding(fullName: FullName, recursivity: Recursivity, body: SIR)

enum CompileDef:
    case Compiling
    case Compiled(binding: TopLevelBinding)

final class SIRCompiler(using ctx: Context) {
    import tpd.*
    import SIRCompiler.Env
    private val DefaultFunSIRBuiltins: Map[Symbol, SIR.Builtin] = Macros.generateBuiltinsMap(ctx)
    private val BigIntSymbol = requiredModule("scala.math.BigInt")
    private val BigIntClassSymbol = requiredClass("scala.math.BigInt")
    private val ByteStringClassSymbol = requiredClass("scalus.builtin.ByteString")
    private val DataClassSymbol = requiredClass("scalus.builtin.Data")
    private val PairSymbol = requiredClass("scalus.builtin.Pair")
    private val ScalusBuiltinListClassSymbol = requiredClass("scalus.builtin.List")
    private val StringContextSymbol = requiredModule("scala.StringContext")
    private val StringContextApplySymbol = StringContextSymbol.requiredMethod("apply")
    private val Tuple2Symbol = requiredClass("scala.Tuple2")
    private val NothingSymbol = defn.NothingClass
    private val NullSymbol = defn.NullClass
    private val ByteStringModuleSymbol = requiredModule("scalus.builtin.ByteString")
    private val ByteStringSymbolHex = ByteStringModuleSymbol.requiredMethod("hex")
    private val ByteStringStringInterpolatorsMethodSymbol =
        ByteStringModuleSymbol.requiredMethod("StringInterpolators")
    private val typer = new SIRTyper
    private val pmCompiler = new PatternMatchingCompiler(this)
    private val sirLoader = new SIRLoader(using ctx)

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

    extension (t: Tree) def isLiteral: Boolean = compileConstant.isDefinedAt(t)
    extension (t: Tree) def isData: Boolean = t.tpe <:< DataClassSymbol.typeRef

    case class LocalBinding(
        name: String,
        symbol: Symbol,
        recursivity: Recursivity,
        body: SIR,
        pos: SourcePosition
    ):
        def fullName(using Context): FullName = FullName(symbol)

    private val globalDefs: mutable.LinkedHashMap[FullName, CompileDef] =
        mutable.LinkedHashMap.empty
    private val globalDataDecls: mutable.LinkedHashMap[FullName, DataDecl] =
        mutable.LinkedHashMap.empty

    case class SuperBinding(
        // here the full name orifinal symbol.
        //  for now, we use the same name.
        name: String,
        // origin symbol (parent of this class) where the method is defined
        parentSymbol: Symbol,
        // child symbol (this class) where the specialized method should be added
        childSymbol: Symbol,
        // specialized body
        body: SIR,
        // is this body changed over original ?
        isChanged: Boolean
    ) {

        def fullName(using Context): FullName =
            FullName(name)

    }
    // private val specializedDefs: mutable.LinkedHashMap[FullName, SIR] =
    //    mutable.LinkedHashMap.empty

    private val CompileAnnot = requiredClassRef("scalus.Compile").symbol.asClass
    private val IgnoreAnnot = requiredClassRef("scalus.Ignore").symbol.asClass

    private def builtinFun(s: Symbol): Option[SIR.Builtin] = {
        DefaultFunSIRBuiltins.get(s)
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

    private def compileTypeDef(td: TypeDef): Unit = {
        val start = System.currentTimeMillis()
        val tpl = td.rhs.asInstanceOf[Template]
        val specializedParents = tpl.parents.flatMap { p =>
            if p.symbol.hasAnnotation(Symbols.requiredClass("scalus.Compile")) then
                if p.symbol.fullName.toString == "scalus.prelude.Validator" then Some(p)
                else if p.symbol.fullName.toString == "scalus.prelude.ParametrizedValidator" then
                    Some(p)
                else throw new RuntimeException("Unsopported parent: " + p.symbol.fullName.toString)
            else None
        }

        val typeParams = td.tpe.typeParams
        val typeParamsSymbols = typeParams.map(_.paramRef.typeSymbol)
        val sirTypeParams = typeParamsSymbols.map { tps =>
            SIRType.TypeVar(tps.name.show, Some(tps.hashCode))
        }
        val sirTypeVars = (typeParamsSymbols zip sirTypeParams).toMap
        val baseEnv = Env.empty.copy(
          thisTypeSymbol = td.symbol,
          typeVars = sirTypeVars
        )

        val bindings = tpl.body.flatMap {
            case dd: DefDef
                if !dd.symbol.flags.is(Flags.Synthetic)
                // uncomment to ignore derived methods
                // && !dd.symbol.name.startsWith("derived")
                    && !dd.symbol.hasAnnotation(IgnoreAnnot) =>
                compileStmt(
                  baseEnv,
                  dd,
                  isGlobalDef = true
                ) match
                    case CompileMemberDefResult.Compiled(b) => Some(b)
                    case _                                  => None
            case vd: ValDef
                if !vd.symbol.flags.isOneOf(Flags.Synthetic | Flags.Case)
                // uncomment to ignore derived methods
                // && !vd.symbol.name.startsWith("derived")
                    && !vd.symbol.hasAnnotation(IgnoreAnnot) =>
                // println(s"valdef: ${vd.symbol.fullName}")
                compileStmt(
                  baseEnv,
                  vd,
                  isGlobalDef = true
                ) match
                    case CompileMemberDefResult.Compiled(b) => Some(b)
                    case _                                  =>
                        // TODO: print diagnostins
                        None
            case _ => None
        }

        // val bindingByName = bindings.map(b => b.name -> b).toMap

        val possibleOverrides = specializedParents.flatMap { p =>
            bindings.map { lb =>
                p.symbol.fullName.show + "." + lb.name
                    -> lb
            }
        }.toMap

        val superBindings = specializedParents.flatMap { p =>
            sirLoader.findAndReadModule(p.symbol.fullName.show, true) match
                case Left(message) =>
                    error(
                      GenericError(
                        s"Builtin module ${p.symbol.showFullName} not found, check is you installatin is complete: ${message}",
                        p.srcPos
                      ),
                      None
                    )
                case Right(module) =>
                    val parentTypeParams = p.tpe.typeParams
                    val parentTypeParamsSymbols = parentTypeParams.map(_.paramRef.typeSymbol)
                    val parentTypeArgs = td.tpe.baseType(p.symbol) match
                        case AppliedType(_, args) =>
                            args.map { a =>
                                sirTypeInEnv(a, p.srcPos, baseEnv)
                            }
                        case _ => Nil
                    val parentTypeVars = (parentTypeParamsSymbols zip parentTypeArgs).toMap
                    val env = baseEnv.copy(typeVars = baseEnv.typeVars ++ parentTypeVars)
                    specializeInModule(
                      p.symbol,
                      module,
                      env,
                      possibleOverrides
                    )
        }

        val nonOverridedSupers = superBindings.filter { b =>
            !possibleOverrides.contains(b.name)
        }

        val superNames = superBindings.map(_.name).toSet

        val bindingsWithSpecialized =
            bindings.map { b =>
                if (superBindings.nonEmpty) then
                    val (newBody, changed) = specializeSIR(
                      Symbols.NoSymbol,
                      b.body,
                      Env.empty.copy(thisTypeSymbol = td.symbol),
                      Map.empty,
                      superNames
                    )
                    Binding(b.fullName.name, newBody)
                else Binding(b.fullName.name, b.body)
            } ++ nonOverridedSupers.map(b => Binding(b.fullName.name, b.body))

        val module =
            Module(
              SIRCompiler.SIRVersion,
              bindingsWithSpecialized
            )
        writeModule(module, td.symbol.fullName.toString)
        val time = System.currentTimeMillis() - start
        report.echo(
          s"compiled Scalus module ${td.name} definitions: ${bindings.map(_.name).mkString(", ")} in ${time}ms"
        )
    }

    private def writeModule(module: Module, className: String): Unit = {
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
        enc.filler()
        output.write(enc.buffer)
        output.close()
    }

    /** Creates [[AdtTypeInfo]] based on a [[Type]].
      *
      * We support these cases:
      *   1. case class Foo(a: Int, b: String) (case 1)
      *   1. case object Bar (case 2)
      *   1. enum Base { case A ...} (case 3)
      *   1. enum Base { case B(a, b) } (case 4)
      *   1. sealed abstract class Base; object Base { case object A extends Base } (case 5)
      *   1. sealed abstract class Base; object Base { case class B(a:Int, b: String) extends Base }
      *      (case 6)
      *   1. scala.Tuple2 (case 7)
      */
    def getAdtTypeInfo(constrTpe: Type): AdtTypeInfo = {
        val typeSymbol = constrTpe.widen.dealias.typeSymbol
        // println(s"getAdtInfoFromConstroctorType: ${typeSymbol.fullName.show}, $constrTpe")
        // look for a base `sealed abstract class`. If it exists, we are in case 5 or 6
        val optAdtBaseTypeSymbol = constrTpe.baseClasses.find(b =>
            // println(s"base class: ${b.show} ${b.flags.flagsString}")
            // TODO:  recheck.  Why ! trait ?
            b.flags.isAllOf(Flags.Sealed | Flags.Abstract) && !b.flags.is(Flags.Trait)
        )

        val typeArgs = constrTpe match
            case AppliedType(_, args) => args
            case _                    => Nil

        if constrTpe.typeConstructor =:= Tuple2Symbol.typeRef
        then AdtTypeInfo(typeSymbol, typeArgs, List(typeSymbol), None)
        else
            optAdtBaseTypeSymbol match
                case None => // case 1 or 2
                    AdtTypeInfo(typeSymbol, typeArgs, List(typeSymbol), None)
                case Some(baseClassSymbol) =>
                    val adtBaseType = constrTpe.baseType(baseClassSymbol)
                    val baseDataParams = adtBaseType match
                        case AppliedType(_, args) => args
                        case _                    => Nil
                    val parents = baseClassSymbol.baseClasses.filter(cf =>
                        cf.children.contains(baseClassSymbol)
                    )
                    val optParent =
                        if parents.isEmpty then None
                        else if parents.size == 1 then parents.headOption
                        else
                            val msg =
                                s"Multiple parents for ${baseClassSymbol.fullName.show}: ${parents.map(_.fullName.show).mkString(", ")}"
                            throw new RuntimeException(msg)
                    AdtTypeInfo(
                      baseClassSymbol,
                      baseDataParams,
                      baseClassSymbol.children,
                      optParent
                    )
    }

    /** Creates [[AdtConstructorCallInfo]] based on a constructor type.
      *
      * We support these cases:
      *   1. case class Foo(a: Int, b: String) (case 1)
      *   1. case object Bar (case 2)
      *   1. enum Base { case A ...} (case 3)
      *   1. enum Base { case B(a, b) } (case 4)
      *   1. sealed abstract class Base; object Base { case object A extends Base } (case 5)
      *   1. sealed abstract class Base; object Base { case class B(a:Int, b: String) extends Base }
      *      (case 6)
      *   1. scala.Tuple2 (case 7)
      */
    private def getAdtConstructorCallInfo(constrTpe: Type): AdtConstructorCallInfo = {
        val typeInfo = getAdtTypeInfo(constrTpe)
        if constrTpe.isSingleton then // case 3, 5
            AdtConstructorCallInfo(constrTpe.termSymbol, typeInfo)
        else // case 1, 2, 4, 6, 7
            val typeSymbol = constrTpe.widen.dealias.typeSymbol
            AdtConstructorCallInfo(typeSymbol, typeInfo)
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

    private def getCachedDataDecl(dataInfo: AdtTypeInfo, env: Env, srcPos: SrcPos): DataDecl = {
        val dataFullName = FullName(dataInfo.dataTypeSymbol)
        // debugInfo(s"compileNewConstructor2: dataTypeSymbol $dataTypeSymbol, dataName $dataName, constrName $constrName, children ${constructors}")
        globalDataDecls.getOrElseUpdate(dataFullName, makeDataDecl(dataInfo, env, srcPos))
    }

    private def makeDataDecl(dataInfo: AdtTypeInfo, env: Env, srcPos: SrcPos) = {
        val dataFullName = FullName(dataInfo.dataTypeSymbol)
        val dataTypeParams = dataInfo.dataTypeParams.map { tp =>
            SIRType.TypeVar(tp.typeSymbol.name.show)
        }
        val constrDecls = dataInfo.constructorsSymbols.map { sym =>
            makeConstrDecl(env, srcPos, sym)
        }
        val sourcePos =
            if (dataInfo.dataTypeSymbol.srcPos.sourcePos == NoSourcePosition) then srcPos.sourcePos
            else dataInfo.dataTypeSymbol.srcPos.sourcePos
        val optComment = dataInfo.dataTypeSymbol.defTree match
            case memberDef: MemberDef =>
                memberDef.rawComment.map(_.raw)
            case _ => None
        val anns = AnnotationsDecl(
          SIRPosition.fromSourcePosition(sourcePos),
          optComment
        )
        scalus.sir.DataDecl(dataFullName.name, constrDecls, dataTypeParams, anns)
    }

    def makeConstrDecl(env: Env, srcPos: SrcPos, constrSymbol: Symbol): ConstrDecl = {
        val typeParams =
            constrSymbol.typeParams.map(tp => SIRType.TypeVar(tp.name.show, Some(tp.hashCode)))
        val envTypeVars1 = constrSymbol.typeParams.foldLeft(env.typeVars) { case (acc, tp) =>
            acc + (tp -> SIRType.TypeVar(tp.name.show, Some(tp.hashCode)))
        }
        val envTypeVars2 = primaryConstructorTypeParams(constrSymbol).foldLeft(envTypeVars1) {
            case (acc, tp) =>
                acc + (tp -> SIRType.TypeVar(tp.name.show, Some(tp.hashCode)))
        }
        val nEnv = env.copy(typeVars = envTypeVars2)
        val params = primaryConstructorParams(constrSymbol).map { p =>
            val pType =
                try sirTypeInEnv(p.info, srcPos, nEnv)
                catch
                    case NonFatal(e) =>
                        println(s"Error in sirTypeInEnv: ${p.info.show} ${p.info.widen.show}")
                        println(
                          s"PrimaryConstructorParams: ${primaryConstructorParams(constrSymbol)}"
                        )
                        println(
                          s"PrimaryConstructorTypeParams: ${primaryConstructorTypeParams(constrSymbol)}"
                        )
                        throw e
            TypeBinding(p.name.show, pType)
        }
        val optBaseClass = constrSymbol.info.baseClasses.find { b =>
            b.flags.is(Flags.Sealed) && b.children.contains(constrSymbol)
        }
        val baseTypeArgs = optBaseClass
            .flatMap { bs =>
                constrSymbol.info.baseType(bs) match
                    case AppliedType(_, args) =>
                        Some(args.map(a => sirTypeInEnv(a, srcPos, nEnv)))
                    case _ => None
            }
            .getOrElse(Nil)
        // TODO: add substoitution for parent type params
        // scalus.sir.ConstrDecl(sym.name.show, SIRVarStorage.DEFAULT, params, typeParams, baseTypeArgs)
        val pos = SIRPosition.fromSrcPos(srcPos)
        val comment = constrSymbol.defTree match
            case memberDef: MemberDef =>
                memberDef.rawComment.map(_.raw)
            case _ => None
        val anns = AnnotationsDecl(pos, comment)
        scalus.sir.ConstrDecl(
          constrSymbol.fullName.show,
          SIRVarStorage.DEFAULT,
          params,
          typeParams,
          baseTypeArgs,
          anns
        )
    }

    private def compileNewConstructor(
        env: Env,
        nakedType: Type,
        fullType: Type,
        args: List[Tree],
        srcPos: SrcPos
    ): SIR = {
        if (nakedType.isGenericTuple) then
            val nArgs = args.size
            if (nArgs == 1 || nArgs == 2) then
                println("Generic tuple with 1 or 2 args,  assume  normal constructpr")
                compileNewConstructorNoTuple(env, nakedType, fullType, args, srcPos)
            else
                val decl = makeGenericTupleDecl(nArgs, srcPos)
                val constrName = s"scala.Tuple${nArgs}"
                val targs = fullType match
                    case AppliedType(nt, targs) =>
                        targs.map(t => sirTypeInEnv(t, srcPos, env)).toList
                    case _ =>
                        error(
                          GenericError(
                            s"Tuple${nArgs} should nave n type arguments, buf fullType is: ${fullType.show}",
                            srcPos
                          ),
                          (1 to nArgs).map(x => SIRType.TypeNothing).toList
                        )
                SIR.Constr(
                  constrName,
                  decl,
                  args.map(compileExpr(env, _)),
                  SIRType.typeApply(decl.constrType(constrName), targs),
                  AnnotationsDecl.fromSrcPos(srcPos)
                )
        else compileNewConstructorNoTuple(env, nakedType, fullType, args, srcPos)

    }

    private def compileNewConstructorNoTuple(
        env: Env,
        nakedType: Type,
        fullType: Type,
        args: List[Tree],
        srcPos: SrcPos
    ): SIR = {
        val constructorCallInfo = getAdtConstructorCallInfo(nakedType)
        val argsE = args.map(compileExpr(env, _))
        val dataDecl = getCachedDataDecl(constructorCallInfo.dataInfo, env, srcPos)
        // constructor body as: constr arg1 arg2 ...
        val (pos, optComment) = constructorCallInfo.dataInfo.constructorsSymbols.find(
          _.fullName.show == constructorCallInfo.fullName
        ) match
            case Some(sym) =>
                sym.defTree match
                    case memberDef: MemberDef =>
                        (SIRPosition.fromSrcPos(memberDef.srcPos), memberDef.rawComment.map(_.raw))
                    case _ =>
                        (SIRPosition.fromSrcPos(srcPos), None)
            case None =>
                report.warning(
                  s"Constructor symbol not found: ${constructorCallInfo.fullName}",
                  srcPos
                )
                (SIRPosition.fromSrcPos(srcPos), None)
        val anns = AnnotationsDecl(pos, optComment)
        SIR.Constr(
          constructorCallInfo.fullName,
          dataDecl,
          argsE,
          sirTypeInEnv(fullType, srcPos, env),
          anns
        )
    }

    // Parameterless case class constructor of an enum
    private def isConstructorVal(symbol: Symbol, @unused tpe: Type): Boolean =
        /* println(
        s"isConstructorVal: ${symbol.flags.isAllOf(Flags.EnumCase)} $symbol: ${tpe.show} <: ${tpe.widen.show}, ${symbol.flagsString}"
      )  */
        symbol.flags.isAllOf(Flags.EnumCase)

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
                                e.symbol.fullName.toString,
                                localType,
                                globalType,
                                e.srcPos
                              ),
                              SIR.Var(
                                e.symbol.fullName.toString,
                                localType,
                                AnnotationsDecl.fromSymIn(e.symbol, e.srcPos.sourcePos)
                              )
                            )
                    case _ =>
                SIR.Var(
                  e.symbol.fullName.toString,
                  localType,
                  AnnotationsDecl.fromSymIn(e.symbol, e.srcPos.sourcePos)
                )
            // local def, use the name
            case (true, false) =>
                SIR.Var(
                  e.symbol.name.show,
                  env.vars(name),
                  AnnotationsDecl.fromSymIn(e.symbol, e.srcPos.sourcePos)
                )
            // global def, use full name
            case (false, true) =>
                SIR.Var(
                  e.symbol.fullName.toString,
                  sirTypeInEnv(e.tpe.widen, e.srcPos, env),
                  AnnotationsDecl.fromSymIn(e.symbol, e.srcPos.sourcePos)
                )
            case (false, false) =>
                // println( s"external var: module ${e.symbol.owner.fullName.toString()}, ${e.symbol.fullName.toString()}" )
                val valType = sirTypeInEnv(e.tpe.widen.dealias, e.srcPos, env)
                try
                    SIR.ExternalVar(
                      e.symbol.owner.fullName.toString,
                      e.symbol.fullName.toString,
                      valType,
                      AnnotationsDecl.fromSymIn(e.symbol, e.srcPos.sourcePos)
                    )
                catch
                    case NonFatal(ex) =>
                        println(s"Error in compileIdentOrQualifiedSelect: ${ex.getMessage}")
                        println(
                          s"ExternalVar: ${e.symbol.fullName} at ${e.srcPos.sourcePos.source.name}:${e.srcPos.line}"
                        )
                        println(s"tree: ${e.show}")
                        throw ex
    }

    enum CompileMemberDefResult {
        case Compiled(b: LocalBinding)
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
        else if vd.symbol.flags.isAllOf(Flags.Lazy, butNot = Flags.Given) then
            error(LazyValNotSupported(vd, vd.srcPos), None)
            CompileMemberDefResult.NotSupported
        // ignore @Ignore annotated statements
        else if vd.symbol.hasAnnotation(IgnoreAnnot) then
            CompileMemberDefResult.Ignored(sirTypeInEnv(vd.tpe, vd.srcPos, env))
        else
            // TODO store comments in the SIR
            // vd.rawComment
            val bodyExpr = compileExpr(env, vd.rhs)
            CompileMemberDefResult.Compiled(
              LocalBinding(name.show, vd.symbol, Recursivity.NonRec, bodyExpr, vd.sourcePos)
            )
    }

    private def compileDefDef(
        env: Env,
        dd: DefDef,
        isGlobalDef: Boolean
    ): CompileMemberDefResult = {
        // ignore inline defs and @Ignore annotated statements
        if dd.symbol.flags.is(Flags.Inline) || dd.symbol.hasAnnotation(IgnoreAnnot) then
            CompileMemberDefResult.Ignored(sirTypeInEnv(dd.tpe, dd.srcPos, env))
        else
            // TODO store comments in the SIR
            // dd.rawComment
            val params = dd.paramss.flatten.collect({ case vd: ValDef => vd })
            val typeParams = dd.paramss.flatten.collect({ case td: TypeDef => td })
            val typeParamsMap = typeParams.foldLeft(Map.empty[Symbol, SIRType]) { case (acc, td) =>
                acc + (td.symbol -> SIRType.TypeVar(td.symbol.name.show, Some(td.symbol.hashCode)))
            }
            val paramVars =
                if params.isEmpty
                then
                    List(
                      SIR.Var("_", SIRType.Unit, AnnotationsDecl.empty)
                    ) /* Param for () argument */
                else
                    params.map { case v: ValDef =>
                        val tEnv =
                            SIRTypeEnv(v.srcPos, env.typeVars ++ typeParamsMap)
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
                        val anns = AnnotationsDecl.fromSymIn(v.symbol, v.srcPos.sourcePos)
                        SIR.Var(v.symbol.name.show, vType, anns)
                    }
            val paramNameTypes = paramVars.map(p => (p.name, p.tp))
            val body = dd.rhs
            val selfName = if isGlobalDef then FullName(dd.symbol).name else dd.symbol.name.show
            val selfType = sirTypeInEnv(dd.tpe, SIRTypeEnv(dd.srcPos, env.typeVars))
            val nTypeVars = env.typeVars ++ typeParamsMap
            val nVars = env.vars ++ paramNameTypes + (selfName -> selfType)
            val bE = compileExpr(env.copy(vars = nVars, typeVars = nTypeVars), body)
            val bodyExpr: scalus.sir.SIR =
                paramVars.foldRight(bE) { (v, acc) =>
                    SIR.LamAbs(v, acc, v.anns)
                }
            CompileMemberDefResult.Compiled(
              LocalBinding(dd.name.show, dd.symbol, Recursivity.Rec, bodyExpr, dd.sourcePos)
            )
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
                  LocalBinding(
                    s"__${stmt.source.file.name.takeWhile(_.isLetterOrDigit)}_line_${stmt.srcPos.line}",
                    NoSymbol,
                    Recursivity.NonRec,
                    compileExpr(env, x),
                    stmt.sourcePos
                  )
                )
    }

    private def compileBlock(env: Env, stmts: immutable.List[Tree], expr: Tree): SIR = {
        if env.debug then println(s"compileBlock: ${stmts.map(_.show).mkString("\n")}")
        val exprs = ListBuffer.empty[LocalBinding]
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
        if env.debug then
            println(s"compileBlock: expr=${expr.show}")
            println(s"compileBlock: exprExprs.tp=${exprExpr.tp.show}")
        val retval = exprs.foldRight(exprExpr) { (bind, sirExpr) =>
            SIR.Let(
              bind.recursivity,
              List(Binding(bind.name, bind.body)),
              sirExpr,
              AnnotationsDecl.fromSourcePosition(expr.sourcePos)
            )
        }
        if env.debug then
            println(
              s"compileBlock: retval.tp=${retval.tp.show}, ${retval.tp} isSumCaseClass=${retval.tp
                      .isInstanceOf[SIRType.SumCaseClass]}"
            )
        retval
    }

    /* Sometimes the compiler leaves Inlined nodes,
     * which we need to skip to get to the actual expression
     * otherwise,say, constants are not compiled correctly
     */
    object SkipInline {
        @tailrec
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
        case t @ Apply(bigintApply, List(SkipInline(literal)))
            if bigintApply.symbol == BigIntSymbol.requiredMethod(
              "apply",
              List(defn.StringClass.typeRef)
            ) =>
            literal match
                case Literal(c) if c.tag == Constants.StringTag =>
                    scalus.uplc.Constant.Integer(BigInt(c.stringValue))
                case _ =>
                    error(
                      GenericError(
                        s"""BigInt(${literal.show}) is not a constant expression.
                             |Try using String literals, like BigInt("123")
                             |""".stripMargin,
                        t.srcPos
                      ),
                      scalus.uplc.Constant.Unit
                    )
        case t @ Apply(bigintApply, List(SkipInline(literal)))
            if bigintApply.symbol == BigIntSymbol.requiredMethod(
              "apply",
              List(defn.IntType)
            ) =>
            literal match
                case Literal(c) if c.tag == Constants.IntTag =>
                    scalus.uplc.Constant.Integer(BigInt(c.intValue))
                case _ =>
                    error(
                      GenericError(
                        s"""BigInt(${literal.show}) is not a constant expression.
                               |Try using Int literals, like BigInt(123)
                               |""".stripMargin,
                        t.srcPos
                      ),
                      scalus.uplc.Constant.Unit
                    )

        case Apply(i, List(SkipInline(literal)))
            if i.symbol == BigIntSymbol.requiredMethod("int2bigInt") =>
            literal match
                case Literal(c) if c.tag == Constants.IntTag =>
                    scalus.uplc.Constant.Integer(BigInt(c.intValue))
                case _ =>
                    error(
                      GenericError(
                        s"""You are trying to implicitly convert an Int expression `${literal.show}` to a BigInt constant.
                               |This is not supported.
                               |Try using Int literals, like BigInt(123) or change the type of the expression `${literal.show}` to BigInt
                               |""".stripMargin,
                        i.srcPos
                      ),
                      scalus.uplc.Constant.Unit
                    )

        case expr if expr.symbol == ByteStringModuleSymbol.requiredMethod("empty") =>
            scalus.uplc.Constant.ByteString(scalus.builtin.ByteString.empty)
        case Apply(expr, List(SkipInline(literal)))
            if expr.symbol == ByteStringModuleSymbol.requiredMethod("fromHex") =>
            literal match
                case Literal(c) if c.tag == Constants.StringTag =>
                    scalus.uplc.Constant.ByteString(
                      scalus.builtin.ByteString.fromHex(c.stringValue)
                    )
                case _ =>
                    error(
                      GenericError(
                        s"""ByteString.fromHex only accepts String literals, like ByteString.fromHex("deadbeef")
                             |But you provided `${literal.show}`, which is not a String literal.
                             |""".stripMargin,
                        expr.srcPos
                      ),
                      scalus.uplc.Constant.Unit
                    )
        case Apply(expr, List(SkipInline(literal)))
            if expr.symbol == ByteStringModuleSymbol.requiredMethod("fromString") =>
            literal match
                case Literal(c) if c.tag == Constants.StringTag =>
                    scalus.uplc.Constant.ByteString(
                      scalus.builtin.ByteString.fromString(c.stringValue)
                    )
                case _ =>
                    error(
                      GenericError(
                        s"""ByteString.fromString only accepts String literals, like ByteString.fromString("deadbeef")
                               |But you provided `${literal.show}`, which is not a String literal.
                               |""".stripMargin,
                        expr.srcPos
                      ),
                      scalus.uplc.Constant.Unit
                    )
        // hex"deadbeef" as ByteString using Scala 3 StringContext extension
        case expr @ Apply(
              Apply(
                byteStringHex,
                List(Apply(stringContextApply, List(SeqLiteral(List(Literal(c)), _))))
              ),
              List(SeqLiteral(List(), _))
            )
            if byteStringHex.symbol == ByteStringSymbolHex
                && stringContextApply.symbol == StringContextApplySymbol =>
            try scalus.uplc.Constant.ByteString(scalus.builtin.ByteString.fromHex(c.stringValue))
            catch
                case NonFatal(e) =>
                    error(
                      GenericError(
                        s"""Hex string `${c.stringValue}` is not a valid hex string.
                           |Make sure it contains only hexadecimal characters (0-9, a-f, A-F)
                           |Error: ${e.getMessage}
                           |""".stripMargin,
                        expr.srcPos
                      ),
                      scalus.uplc.Constant.Unit
                    )
        // hex"deadbeef" as ByteString for Scala 2 implicit StringInterpolators
        case expr @ Apply(
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
            try
                scalus.uplc.Constant.ByteString(
                  scalus.builtin.ByteString.fromHex(const.stringValue)
                )
            catch
                case NonFatal(e) =>
                    error(
                      GenericError(
                        s"""Hex string `${const.stringValue}` is not a valid hex string.
                               |Make sure it contains only hexadecimal characters (0-9, a-f, A-F)
                               |Error: ${e.getMessage}
                               |""".stripMargin,
                        expr.srcPos
                      ),
                      scalus.uplc.Constant.Unit
                    )

    }

    private def typeReprToDefaultUni(tpe: Type, list: Tree): DefaultUni =
        if tpe =:= BigIntClassSymbol.typeRef then DefaultUni.Integer
        else if tpe =:= defn.StringClass.typeRef then DefaultUni.String
        else if tpe =:= defn.BooleanClass.typeRef then DefaultUni.Bool
        else if tpe =:= defn.UnitClass.typeRef then DefaultUni.Unit
        else if tpe =:= DataClassSymbol.typeRef then DefaultUni.Data
        else if tpe =:= ByteStringClassSymbol.typeRef then DefaultUni.ByteString
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
                    SIRType.Integer ->: SIRType.Integer,
                    AnnotationsDecl.fromSourcePosition(lhs.srcPos.sourcePos)
                  ),
                  compileExpr(env, rhs),
                  SIRType.Integer,
                  AnnotationsDecl.fromSourcePosition(lhs.sourcePos union rhs.sourcePos)
                )
            case nme.MINUS =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.subtractInteger,
                    compileExpr(env, lhs),
                    SIRType.Integer ->: SIRType.Integer,
                    AnnotationsDecl.fromSourcePosition(lhs.sourcePos)
                  ),
                  compileExpr(env, rhs),
                  SIRType.Integer,
                  AnnotationsDecl.fromSourcePosition(lhs.sourcePos union rhs.sourcePos)
                )
            case nme.MUL =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.multiplyInteger,
                    compileExpr(env, lhs),
                    SIRType.Integer ->: SIRType.Integer,
                    AnnotationsDecl.fromSourcePosition(lhs.sourcePos)
                  ),
                  compileExpr(env, rhs),
                  SIRType.Integer,
                  AnnotationsDecl.fromSourcePosition(lhs.sourcePos union rhs.sourcePos)
                )
            case nme.DIV =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.divideInteger,
                    compileExpr(env, lhs),
                    SIRType.Integer ->: SIRType.Integer,
                    AnnotationsDecl.fromSourcePosition(lhs.sourcePos)
                  ),
                  compileExpr(env, rhs),
                  SIRType.Integer,
                  AnnotationsDecl.fromSourcePosition(lhs.sourcePos union rhs.sourcePos)
                )
            case nme.MOD =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.remainderInteger,
                    compileExpr(env, lhs),
                    SIRType.Integer ->: SIRType.Integer,
                    AnnotationsDecl.fromSourcePosition(lhs.sourcePos)
                  ),
                  compileExpr(env, rhs),
                  SIRType.Integer,
                  AnnotationsDecl.fromSourcePosition(lhs.sourcePos union rhs.sourcePos)
                )
            case nme.LT =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.lessThanInteger,
                    compileExpr(env, lhs),
                    SIRType.Integer ->: SIRType.Boolean,
                    AnnotationsDecl.fromSourcePosition(lhs.sourcePos)
                  ),
                  compileExpr(env, rhs),
                  SIRType.Boolean,
                  AnnotationsDecl.fromSourcePosition(lhs.sourcePos union rhs.sourcePos)
                )
            case nme.LE =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.lessThanEqualsInteger,
                    compileExpr(env, lhs),
                    SIRType.Integer ->: SIRType.Boolean,
                    AnnotationsDecl.fromSourcePosition(lhs.sourcePos)
                  ),
                  compileExpr(env, rhs),
                  SIRType.Boolean,
                  AnnotationsDecl.fromSourcePosition(lhs.sourcePos union rhs.sourcePos)
                )
            case nme.GT =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.lessThanInteger,
                    compileExpr(env, rhs),
                    SIRType.Integer ->: SIRType.Boolean,
                    AnnotationsDecl.fromSourcePosition(rhs.sourcePos)
                  ),
                  compileExpr(env, lhs),
                  SIRType.Boolean,
                  AnnotationsDecl.fromSourcePosition(lhs.sourcePos union rhs.sourcePos)
                )
            case nme.GE =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.lessThanEqualsInteger,
                    compileExpr(env, rhs),
                    SIRType.Integer ->: SIRType.Boolean,
                    AnnotationsDecl.fromSourcePosition(rhs.sourcePos)
                  ),
                  compileExpr(env, lhs),
                  SIRType.Boolean,
                  AnnotationsDecl.fromSourcePosition(lhs.sourcePos union rhs.sourcePos)
                )
            case nme.EQ =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.equalsInteger,
                    compileExpr(env, lhs),
                    SIRType.Integer ->: SIRType.Boolean,
                    AnnotationsDecl.fromSourcePosition(lhs.sourcePos)
                  ),
                  compileExpr(env, rhs),
                  SIRType.Boolean,
                  AnnotationsDecl.fromSourcePosition(lhs.sourcePos union rhs.sourcePos)
                )
            case nme.NE =>
                val anns = AnnotationsDecl.fromSourcePosition(lhs.sourcePos union rhs.sourcePos)
                SIR.Not(
                  SIR.Apply(
                    SIR.Apply(
                      SIRBuiltins.equalsInteger,
                      compileExpr(env, lhs),
                      SIRType.Integer ->: SIRType.Boolean,
                      AnnotationsDecl.fromSourcePosition(lhs.sourcePos)
                    ),
                    compileExpr(env, rhs),
                    SIRType.Boolean,
                    anns
                  ),
                  anns
                )
            case _ =>
                error(
                  UnsupportedBigIntOp(op.show, optree.srcPos),
                  SIR.Error(
                    "Unsupported BigInt operation",
                    AnnotationsDecl.fromSourcePosition(optree.sourcePos)
                  )
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
                        SIR.Apply(
                          SIRBuiltins.fstPair,
                          expr,
                          t1,
                          AnnotationsDecl.fromSourcePosition(tree.sourcePos)
                        )
                    case other =>
                        error(
                          TypeMismatch(
                            fun.toString,
                            SIRType.Pair(SIRType.TypeVar("A"), SIRType.TypeVar("B")),
                            other,
                            tree.srcPos
                          ),
                          SIR.Error(
                            "Type mismatch",
                            AnnotationsDecl.fromSourcePosition(tree.sourcePos)
                          )
                        )
            case "snd" =>
                val expr = compileExpr(env, pair)
                expr.tp match
                    case SIRType.Pair(t1, t2) =>
                        SIR.Apply(
                          SIRBuiltins.sndPair,
                          expr,
                          t2,
                          AnnotationsDecl.fromSourcePosition(tree.sourcePos)
                        )
                    case other =>
                        error(
                          TypeMismatch(
                            fun.toString,
                            SIRType.Pair(SIRType.TypeVar("A"), SIRType.TypeVar("B")),
                            other,
                            tree.srcPos
                          ),
                          SIR.Error("", AnnotationsDecl.fromSourcePosition(tree.sourcePos))
                        )
            case _ =>
                error(
                  UnsupportedPairFunction(fun.toString, tree.srcPos),
                  SIR.Error("", AnnotationsDecl.fromSourcePosition(tree.sourcePos))
                )

    private def compileBuiltinPairConstructor(
        env: Env,
        a: Tree,
        b: Tree,
        tpe1: Tree,
        tpe2: Tree,
        tree: Tree
    ): SIR =
        if env.debug then
            println(
              s"compileBuiltinPairConstructor: ${a.show}, ${b.show}, tpe1: $tpe1, tpe2: $tpe2"
            )
        // We can create a Pair by either 2 literals as (con pair...)
        // or 2 Data variables using MkPairData builtin
        if a.isLiteral && b.isLiteral then
            SIR.Const(
              scalus.uplc.Constant.Pair(compileConstant(a), compileConstant(b)),
              SIRType.Pair(sirTypeInEnv(a.tpe, a.srcPos, env), sirTypeInEnv(b.tpe, b.srcPos, env)),
              AnnotationsDecl.fromSrcPos(tree.srcPos)
            )
        else if a.isData && b.isData then
            val exprA = compileExpr(env, a)
            val exprB = compileExpr(env, b)
            // val typeB = SIRType.TypeVar("B")
            SIR.Apply(
              SIR.Apply(
                SIRBuiltins.mkPairData,
                exprA,
                SIRType.Fun(exprB.tp, SIRType.Pair(exprA.tp, exprB.tp)),
                AnnotationsDecl.fromSrcPos(a.srcPos)
              ),
              exprB,
              SIRType.Pair(exprA.tp, exprB.tp),
              AnnotationsDecl.fromSrcPos(tree.srcPos)
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
              SIR.Error("", AnnotationsDecl.fromSrcPos(tree.srcPos))
            )

    private def compileBuiltinListMethods(
        env: Env,
        lst: Tree,
        fun: Name,
        targs: List[Tree],
        tree: Tree
    ): SIR =
        if env.debug then
            println(s"compileBuiltinListMethods: ${lst.show}, fun: $fun, targs: $targs")
        val posAnns = AnnotationsDecl.fromSrcPos(tree.srcPos)
        fun.show match
            case "head" =>
                val exprA = compileExpr(env, lst)
                exprA.tp match
                    case SIRType.List(t) =>
                        SIR.Apply(
                          SIRBuiltins.headList,
                          exprA,
                          t,
                          posAnns
                        )
                    case other =>
                        println(s"expected that exprA.tp ${exprA} is List, but got: ${other}")
                        throw new Exception("expected that exprA.tp is List")
                        error(
                          TypeMismatch(
                            fun.toString,
                            SIRType.List(SIRType.TypeVar("A")),
                            other,
                            tree.srcPos
                          ),
                          SIR.Error("", posAnns)
                        )
            case "tail" =>
                val exprArg = compileExpr(env, lst)
                SIR.Apply(
                  SIRBuiltins.tailList,
                  exprArg,
                  exprArg.tp,
                  posAnns
                )
            case "isEmpty" =>
                SIR.Apply(
                  SIRBuiltins.nullList,
                  compileExpr(env, lst),
                  SIRType.Boolean,
                  posAnns
                )
            case _ =>
                error(UnsupportedListFunction(fun.toString, lst.srcPos), SIR.Error("", posAnns))

    private def compileBuiltinListConstructor(
        env: Env,
        ex: Tree,
        list: Tree,
        tpe: Tree,
        tree: Tree
    ): SIR =
        if env.debug then
            println(s"compileBuiltinListConstructor: ${ex.show}, list: $list, tpe: $tpe")
        val tpeE = typeReprToDefaultUni(tpe.tpe, list)
        val tpeTp = sirTypeInEnv(tpe.tpe, tree.srcPos, env)
        val listTp = SIRType.List(tpeTp)
        val anns = AnnotationsDecl.fromSrcPos(tree.srcPos)
        if env.debug then
            println(
              s"compileBuiltinListConstructor: tpeE: $tpeE, tpeTp: $tpeTp, listTp: $listTp, listTp.show=${listTp.show}"
            )
        ex match
            case SeqLiteral(args, _) =>
                val allLiterals = args.forall(arg => compileConstant.isDefinedAt(arg))
                if allLiterals then
                    val lits = args.map(compileConstant)
                    SIR.Const(
                      scalus.uplc.Constant.List(tpeE, lits),
                      listTp,
                      AnnotationsDecl.fromSrcPos(list.srcPos)
                    )
                else
                    val nil: SIR = SIR.Const(
                      scalus.uplc.Constant.List(tpeE, Nil),
                      SIRType.List.Nil,
                      AnnotationsDecl.fromSrcPos(ex.srcPos)
                    )
                    val retval = args.foldRight(nil) { (arg, acc) =>
                        SIR.Apply(
                          SIR.Apply(
                            SIRBuiltins.mkCons,
                            compileExpr(env, arg),
                            SIRType.Fun(listTp, listTp),
                            anns
                          ),
                          acc,
                          listTp,
                          anns
                        )
                    }
                    if env.debug then
                        println(s"compileBuiltinListConstructor: retval: $retval")
                        println(s"compileBuiltinListConstructor: retval.tp: ${retval.tp.show}")
                    retval
            case _ =>
                error(UnsupportedListApplyInvocation(tree, tpe, tree.srcPos), SIR.Error("", anns))

    private def compileApply(
        env0: Env,
        f: Tree,
        targs: List[Tree],
        args: List[Tree],
        applyTpe: Type,
        applyTree: Apply
    ): SIR = {
        if env0.debug then
            println(
              s"compileApply: ${f.show}, targs: $targs, args: $args, applyTpe: $applyTpe, applyTree: $applyTree"
            )
        val env = fillTypeParamInTypeApply(f.symbol, targs, env0)
        val fE = compileExpr(env, f)
        val applySirType = sirTypeInEnv(applyTpe, applyTree.srcPos, env)
        val argsE = args.map(compileExpr(env, _))
        val applyAnns = AnnotationsDecl.fromSrcPos(applyTree.srcPos)
        if argsE.isEmpty then
            SIR.Apply(
              fE,
              SIR.Const(scalus.uplc.Constant.Unit, SIRType.Unit, applyAnns),
              applySirType,
              applyAnns
            )
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
                          SIRType.Unit
                        )
                (SIR.Apply(fun, arg, nTp, applyAnns), nTp)
            }
            applyExpr
    }

    /** Compile a throw expression to SIR
      *
      * {{{
      *    throw new Exception("error msg") // becomes SIR.Error("error msg")
      *    throw new CustomException // becomes SIR.Error("CustomException")
      *    throw foo() // becomes SIR.Error("foo()")
      *    inline def err(inline msg: String) = throw new RuntimeException(msg)
      *    err("test") // becomes SIR.Error("test")
      * }}}
      */
    private def compileThrowException(ex: Tree): SIR =
        val msg = ex match
            case SkipInline(
                  Apply(
                    Select(New(tpt), nme.CONSTRUCTOR),
                    immutable.List(SkipInline(arg), _*)
                  )
                ) if tpt.tpe <:< defn.ThrowableType =>
                arg match
                    case Literal(c) => c.stringValue
                    case _ =>
                        report.warning(
                          s"""Only string literals are supported as exception messages, but found ${arg.show}.
                          |Try rewriting the code to use a string literal like `throw new RuntimeException("error message")`
                          |Scalus will compile this to `Error("${arg.show}")`.""".stripMargin,
                          ex.srcPos
                        )
                        arg.show
            case SkipInline(Apply(Select(New(tpt), nme.CONSTRUCTOR), Nil))
                if tpt.tpe <:< defn.ThrowableType =>
                tpt.symbol.showName
            case SkipInline(term) =>
                term.show
        SIR.Error(msg, AnnotationsDecl.fromSrcPos(ex.srcPos))

    private def compileEquality(env: Env, lhs: Tree, op: Name, rhs: Tree, srcPos: SrcPos): SIR = {
        lazy val lhsExpr = compileExpr(env, lhs)
        lazy val rhsExpr = compileExpr(env, rhs)
        val lhsTpe = lhs.tpe.widen.dealias
        val posAnns = AnnotationsDecl.fromSrcPos(srcPos)
        if lhsTpe =:= BigIntClassSymbol.typeRef then
            // common mistake: comparing BigInt with Int literal, e.g. BigInt(1) == 1
            rhs match
                case Literal(l) =>
                    report.error(
                      s"""You are trying to compare a BigInt and non-BigInt literal:
                           |  ${lhs.show} ${op.show} ${rhs.show}.
                           |
                           |Convert the literal to BigInt before comparing:
                           |  ${lhs.show} ${op} BigInt(${l.show})
                           |""".stripMargin,
                      rhs.srcPos
                    )
                    SIR.Error(
                      "Equality is only allowed between the same types",
                      posAnns
                    )
                case _ =>
                    val eq =
                        SIR.Apply(
                          SIR.Apply(
                            SIRBuiltins.equalsInteger,
                            lhsExpr,
                            SIRType.Integer ->: SIRType.Boolean,
                            AnnotationsDecl.fromSrcPos(lhs.srcPos)
                          ),
                          rhsExpr,
                          SIRType.Boolean,
                          posAnns
                        )
                    if op == nme.EQ then eq else SIR.Not(eq, posAnns)
        else if lhsTpe =:= defn.BooleanType then
            if op == nme.EQ then
                SIR.IfThenElse(
                  lhsExpr,
                  rhsExpr,
                  SIR.IfThenElse(
                    rhsExpr,
                    SIR.Const(
                      scalus.uplc.Constant.Bool(false),
                      SIRType.Boolean,
                      posAnns
                    ),
                    SIR.Const(
                      scalus.uplc.Constant.Bool(true),
                      SIRType.Boolean,
                      posAnns
                    ),
                    SIRType.Boolean,
                    posAnns
                  ),
                  SIRType.Boolean,
                  posAnns
                )
            else
                SIR.IfThenElse(
                  lhsExpr,
                  SIR.IfThenElse(
                    rhsExpr,
                    SIR.Const(scalus.uplc.Constant.Bool(false), SIRType.Boolean, posAnns),
                    SIR.Const(scalus.uplc.Constant.Bool(true), SIRType.Boolean, posAnns),
                    SIRType.Boolean,
                    posAnns
                  ),
                  rhsExpr,
                  SIRType.Boolean,
                  posAnns
                )
        else if lhsTpe =:= ByteStringClassSymbol.typeRef then
            val eq =
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.equalsByteString,
                    lhsExpr,
                    SIRType.ByteString ->: SIRType.Boolean,
                    posAnns
                  ),
                  rhsExpr,
                  SIRType.Boolean,
                  posAnns
                )
            if op == nme.EQ then eq else SIR.Not(eq, posAnns)
        else if lhsTpe =:= defn.StringClass.typeRef then
            val eq =
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.equalsString,
                    lhsExpr,
                    SIRType.String ->: SIRType.Boolean,
                    posAnns
                  ),
                  rhsExpr,
                  SIRType.Boolean,
                  posAnns
                )
            if op == nme.EQ then eq else SIR.Not(eq, posAnns)
        else if lhsTpe <:< DataClassSymbol.typeRef && !(lhsTpe =:= NothingSymbol.typeRef || lhsTpe =:= NullSymbol.typeRef)
        then
            val eq =
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.equalsData,
                    lhsExpr,
                    SIRType.Fun(SIRType.Data, SIRType.Boolean),
                    posAnns
                  ),
                  rhsExpr,
                  SIRType.Boolean,
                  posAnns
                )
            if op == nme.EQ then eq else SIR.Not(eq, posAnns)
        else
            report.error(
              s"""Equality check operations (`==`, `!=`) are only allowed between these primitive types:
                 | BigInt, Boolean, ByteString, String, Data
                 |
                 |You are trying to compare these types:
                 |  ${lhs.tpe.widen.show} ${op} ${rhs.tpe.widen.show}
                 |  ---------- dealiaing to ------------
                 |  ${lhs.tpe.widen.dealias.show} ${op} ${rhs.tpe.widen.dealias.show}
                 |
                 |Make sure you compare values of the same supported type.
                 |
                 |If you want to compare data types try using `===` operator from Prelude.
                 |
                 |Or convert the data to a supported type before comparing,
                 |e.g. toData(x) == toData(y)
                 |
                 |""".stripMargin,
              srcPos
            )
            SIR.Error("Equality is only allowed between the same types", posAnns)
    }

    def compileExpr[T](env: Env, tree: Tree)(using Context): SIR = {
        if env.debug then println(s"compileExpr: ${tree.showIndented(2)}, env: $env")
        if compileConstant.isDefinedAt(tree) then
            val const = compileConstant(tree)
            SIR.Const(
              const,
              sirTypeInEnv(tree.tpe, tree.srcPos, env),
              AnnotationsDecl.fromSrcPos(tree.srcPos)
            )
        else compileExpr2(env.copy(level = env.level + 1), tree)
    }

    private def compileExpr2(env: Env, tree: Tree)(using Context): SIR = {
        tree match
            case If(cond, t, f) =>
                if env.debug then println(s"compileExpr2: If ${cond.show}, ${t.show}, ${f.show}")
                val nEnv = env.copy(level = env.level + 1)
                val ct = compileExpr(nEnv, t)
                val cf = compileExpr(nEnv, f)
                val sirTp = sirTypeInEnv(tree.tpe, tree.srcPos, env)
                SIR.IfThenElse(
                  compileExpr(nEnv, cond),
                  ct,
                  cf,
                  sirTp,
                  AnnotationsDecl.fromSrcPos(tree.srcPos)
                )
            case m: Match => compileMatch(m, env)
            // throw new Exception("error msg")
            // Supports any exception type that uses first argument as message
            case Apply(Ident(nme.throw_), immutable.List(ex)) => compileThrowException(ex)
            // Boolean
            case Select(lhs, op) if lhs.tpe.widen =:= defn.BooleanType && op == nme.UNARY_! =>
                val lhsExpr = compileExpr(env, lhs)
                SIR.Not(lhsExpr, AnnotationsDecl.fromSrcPos(tree.srcPos))
            case Apply(Select(lhs, op), List(rhs))
                if lhs.tpe.widen =:= defn.BooleanType && op == nme.ZAND =>
                val lhsExpr = compileExpr(env, lhs)
                val rhsExpr = compileExpr(env, rhs)
                SIR.And(lhsExpr, rhsExpr, AnnotationsDecl.fromSrcPos(tree.srcPos))
            case Apply(Select(lhs, op), List(rhs))
                if lhs.tpe.widen =:= defn.BooleanType && op == nme.ZOR =>
                val lhsExpr = compileExpr(env, lhs)
                val rhsExpr = compileExpr(env, rhs)
                SIR.Or(lhsExpr, rhsExpr, AnnotationsDecl.fromSrcPos(tree.srcPos))
            // Equality and inequality: ==, !=
            case Apply(Select(lhs, op), List(rhs)) if op == nme.EQ || op == nme.NE =>
                compileEquality(env, lhs, op, rhs, tree.srcPos)
            // BUILTINS
            case bi: Select if builtinFun(bi.symbol).isDefined =>
                if env.debug then println(s"compileExpr: builtinFun: ${bi.symbol}")
                builtinFun(bi.symbol).get
            case bi: Ident if builtinFun(bi.symbol).isDefined =>
                builtinFun(bi.symbol).get
            // BigInt stuff
            case Apply(optree @ Select(lhs, op), List(rhs))
                if lhs.tpe.widen =:= BigIntClassSymbol.typeRef =>
                compileBigIntOps(env, lhs, op, rhs, optree)
            case Select(expr, op)
                if expr.tpe.widen =:= BigIntClassSymbol.typeRef && op == nme.UNARY_- =>
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.subtractInteger,
                    SIR.Const(
                      scalus.uplc.Constant.Integer(BigInt(0)),
                      SIRType.Integer,
                      AnnotationsDecl.empty
                    ),
                    SIRType.Integer ->: SIRType.Integer,
                    AnnotationsDecl.fromSourcePosition(tree.sourcePos)
                  ),
                  compileExpr(env, expr),
                  SIRType.Integer,
                  AnnotationsDecl.fromSrcPos(tree.srcPos)
                )
            // List BUILTINS
            case TypeApply(Select(lst, fun), targs) if lst.isList =>
                compileBuiltinListMethods(env, lst, fun, targs, tree)
            case Select(lst, fun) if lst.isList =>
                compileBuiltinListMethods(env, lst, fun, Nil, tree)
            case tree @ TypeApply(Select(list, name), immutable.List(tpe))
                if name == termName("empty") && list.tpe =:= requiredModule(
                  "scalus.builtin.List"
                ).typeRef =>
                val tpeE = typeReprToDefaultUni(tpe.tpe, tree)
                SIR.Const(
                  scalus.uplc.Constant.List(tpeE, Nil),
                  SIRType.List(sirTypeInEnv(tpe.tpe, tree.srcPos, env)),
                  AnnotationsDecl.fromSrcPos(tree.srcPos)
                )
            case Apply(
                  TypeApply(Select(list, name), immutable.List(tpe)),
                  immutable.List(arg)
                ) if name == termName("::") && list.isList =>
                val argE = compileExpr(env, arg)
                val exprType = sirTypeInEnv(tree.tpe, tree.srcPos, env)
                SIR.Apply(
                  SIR.Apply(
                    SIRBuiltins.mkCons,
                    argE,
                    SIRType.Fun(exprType, exprType),
                    AnnotationsDecl.fromSrcPos(arg.srcPos)
                  ),
                  compileExpr(env, list),
                  exprType,
                  AnnotationsDecl.fromSrcPos(tree.srcPos)
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
            case Apply(TypeApply(con @ Select(f, nme.CONSTRUCTOR), targs), args) =>
                compileNewConstructor(env, f.tpe, tree.tpe.widen, args, tree)
            case Apply(con @ Select(f, nme.CONSTRUCTOR), args) =>
                compileNewConstructor(env, f.tpe, tree.tpe.widen, args, tree)
            // (a, b) as scala.Tuple2.apply(a, b)
            // we need to special-case it because we use scala-library 2.13.x
            // which does not include TASTy so we can't access the method body
            case Apply(TypeApply(app @ Select(f, nme.apply), targs), args)
                if app.symbol.fullName.show == "scala.Tuple2$.apply" =>
                compileNewConstructor(env, tree.tpe, tree.tpe.widen, args, tree)
            case Apply(app @ Select(f, nme.apply), args)
                if app.symbol.fullName.show == "scala.Tuple2$.apply" =>
                compileNewConstructor(env, tree.tpe, tree.tpe, args, tree)
            /* case class Test(a: Int)
             * val t = Test(42)
             * is translated to
             * val t = Test.apply(42), where Test.apply is a synthetic method of a companion object
             * We need to compile it as a primary constructor
             */
            case Apply(TypeApply(apply, targs), args)
                if apply.symbol.flags
                    .is(Flags.Synthetic) && apply.symbol.owner.flags.is(Flags.ModuleClass)
                    && apply.symbol.name.toString == "apply" =>
                val classSymbol: Symbol = apply.symbol.owner.linkedClass
                try compileNewConstructor(env, classSymbol.typeRef, tree.tpe.widen, args, tree)
                catch
                    case ex: RuntimeException =>
                        println(
                          s"exception in compileNewConstructor, classSymbol [Apply.owner.linkedClass]: ${classSymbol} "
                        )
                        println(
                          s"apply.symbol.name=${apply.symbol.name}"
                        )
                        println(
                          s"classSymbol.typeRef=${classSymbol.typeRef}, classSymbol.isType=${classSymbol.isType}"
                        )
                        println(
                          s"classSymbol.typeRef.isGenericTuple=${classSymbol.typeRef.isGenericTuple}"
                        )
                        throw ex

            case Apply(apply @ Select(f, nme.apply), args)
                if apply.symbol.flags
                    .is(Flags.Synthetic) && apply.symbol.owner.flags.is(Flags.ModuleClass) =>
                // get a class symbol from a companion object
                val classSymbol: Symbol = apply.symbol.owner.linkedClass
                compileNewConstructor(env, classSymbol.typeRef, tree.tpe.widen, args, tree)
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
                    compileNewConstructor(env, tree.tpe, tree.tpe, Nil, tree)
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
                    SIR.Select(lhs, ident.show, selType, AnnotationsDecl.fromSrcPos(sel.srcPos))
                // else if obj.symbol.isPackageDef then
                // compileExpr(env, obj)
                else if isConstructorVal(tree.symbol, tree.tpe) then
                    compileNewConstructor(env, tree.tpe, tree.tpe, Nil, tree.srcPos)
                else if isVirtualCall(tree, obj.symbol, ident) then
                    compileVirtualCall(env, tree, obj, ident)
                else compileIdentOrQualifiedSelect(env, tree)
            // ignore asInstanceOf
            case TypeApply(Select(e, nme.asInstanceOf_), _) =>
                compileExpr(env, e)
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
                          SIR.Error("Ignored closure", AnnotationsDecl.fromSrcPos(tree.srcPos))
                        )
                    case CompileMemberDefResult.Builtin(name, tp) =>
                        error(
                          GenericError("Builtin library can be part of user code", tree.srcPos),
                          SIR.Error("Builtin definition", AnnotationsDecl.fromSrcPos(tree.srcPos))
                        )
                    case _ =>
                        // assume,  that if we have here unsupported, error is already reported.
                        SIR.Error(
                          "Closure with not supported form",
                          AnnotationsDecl.fromSrcPos(tree.srcPos)
                        )
            case Block(stmt, expr) => compileBlock(env, stmt, expr)
            case Typed(expr, _)    => compileExpr(env, expr)
            case Inlined(_, bindings, expr) =>
                val r = compileBlock(env, bindings, expr)
                // val t = r.asTerm.show
                // report.info(s"Inlined: ${bindings}, ${expr.show}\n${t}", Position(SourceFile.current, globalPosition, 0))
                r
            case Return(expr, _) =>
                error(
                  ReturnNotSupported(expr, tree.srcPos),
                  SIR.Error("Return not supported", AnnotationsDecl.fromSrcPos(tree.srcPos))
                )
            case Assign(lhs, rhs) =>
                error(
                  ExpressionNotSupported("Variable assignment", tree.srcPos),
                  SIR.Error(
                    "Unsupported assign expression",
                    AnnotationsDecl.fromSrcPos(tree.srcPos)
                  )
                )
            case Try(_, _, _) =>
                error(
                  ExpressionNotSupported("'try-catch-finally'", tree.srcPos),
                  SIR.Error("Unsupported try expression", AnnotationsDecl.fromSrcPos(tree.srcPos))
                )
            case WhileDo(cond, body) =>
                error(
                  ExpressionNotSupported("'while' expression", tree.srcPos),
                  SIR.Error("Unsupported while expression", AnnotationsDecl.fromSrcPos(tree.srcPos))
                )
            case x =>
                error(
                  ExpressionNotSupported(x.show, tree.srcPos),
                  SIR.Error("Unsupported expression", AnnotationsDecl.fromSrcPos(tree.srcPos))
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

    def compileToSIR(tree: Tree, debug: Boolean): SIR = {
        if (debug) {
            println(s"compileToSIR: ${tree.show}")
        }
        compileExpr(Env.empty.copy(debug = debug), tree)
    }

    // def sirType(tp: Type, srcPos: SrcPos): SIRType = {
    //        sirTypeInEnv(tp, SIRTypesHelper.SIRTypeEnv(srcPos, Map.empty))
    // }

    def sirTypeInEnv(tp: Type, srcPos: SrcPos, env: Env): SIRType = {
        sirTypeInEnv(tp, SIRTypeEnv(srcPos, env.typeVars))
    }

    protected def sirTypeInEnv(tp: Type, env: SIRTypeEnv): SIRType = {
        typer.sirTypeInEnv(tp, env)
    }

    private def isVirtualCall(tree: Tree, qualifierSym: Symbol, name: Name): Boolean = {
        val declDenotation = qualifierSym.info.decl(name)
        !declDenotation.exists
    }

    private def compileVirtualCall(env: Env, tree: Tree, qualifier: Tree, name: Name): SIR = {
        if env.debug then println("compile virtual call: " + tree.show)
        val qualifierSym = qualifier.symbol
        val qualifierTypeSym = qualifier.tpe.typeSymbol
        val member = qualifierSym.info.member(name)
        if (!member.exists) then
            error(
              GenericError(s"Member ${name.show} not found in ${qualifierSym.show}", tree.srcPos),
              SIR.Error("Member not found", AnnotationsDecl.fromSrcPos(tree.srcPos))
            )
        else
            member.info match
                case _: MethodType | _: PolyType =>
                    SIR.ExternalVar(
                      qualifierTypeSym.fullName.toString,
                      member.symbol.fullName.toString,
                      sirTypeInEnv(member.info.finalResultType, tree.srcPos, env),
                      AnnotationsDecl.fromSrcPos(tree.srcPos)
                    )
                case _ =>
                    error(
                      GenericError(
                        s"Overriden method in ${qualifierSym.show} should be a method or a polytype",
                        tree.srcPos
                      ),
                      SIR.Error("Invalid overriding", AnnotationsDecl.fromSrcPos(tree.srcPos))
                    )
    }

    private def makeGenericTupleDecl(size: Int, srcPos: SrcPos): DataDecl = {
        // we will generate tupleN decl for each type.
        //  (because we have no generic tupel with AnyKind in SIR)
        //  theoreticalle we can add all to "scala.::*" as in scala.
        //  but not sure if exists use-case when it needed.
        //  (case over generic tuple with different arity in the user program)
        val tupleName = s"scala.Tuple$size"
        this.globalDataDecls.get(FullName(tupleName)) match
            case Some(decl) =>
                decl
            case None =>
                val tupleNameHash = tupleName.hashCode
                val tupleTypeParams =
                    (1 to size)
                        .map(i => SIRType.TypeVar(s"T$i", Some(tupleNameHash + i * 5)))
                        .toList
                val tupleParams =
                    tupleTypeParams.map(t => TypeBinding(t.name.toLowerCase, t)).toList
                val tupleDecl = DataDecl(
                  tupleName,
                  List(
                    ConstrDecl(
                      tupleName,
                      LocalUPLC,
                      tupleParams,
                      tupleTypeParams,
                      tupleTypeParams,
                      AnnotationsDecl.fromSrcPos(srcPos)
                    )
                  ),
                  tupleTypeParams,
                  AnnotationsDecl.fromSrcPos(srcPos)
                )
                this.globalDataDecls.put(FullName(tupleName), tupleDecl)
                tupleDecl
    }

    private def specializeInModule(
        parentSym: Symbol,
        module: scalus.sir.Module,
        env: Env,
        possibleOverrides: Map[String, LocalBinding]
    ): List[SuperBinding] = {
        val thisSymbol = env.thisTypeSymbol
        val thisClassNames = module.defs.map(_.name).toSet
        for {
            binding <- module.defs
        } yield {
            val (nSIR, isChanged) =
                specializeSIR(parentSym, binding.value, env, possibleOverrides, thisClassNames)
            SuperBinding(
              binding.name,
              parentSym,
              env.thisTypeSymbol,
              nSIR,
              isChanged
            )
        }
    }

    private def specializeSIR(
        parentSym: Symbol,
        sir: SIR,
        env: Env,
        possibleOverrides: Map[String, LocalBinding],
        thisClassNames: Set[String]
    ): (SIR, Boolean) = {
        sir match
            case SIR.ExternalVar(moduleName, name, tp, anns) =>
                possibleOverrides.get(name) match
                    case Some(binding) =>
                        SIR.ExternalVar(
                          env.thisTypeSymbol.fullName.toString,
                          binding.fullName.name,
                          tp,
                          anns
                        ) -> true
                    case None =>
                        if thisClassNames.contains(name) then
                            SIR.ExternalVar(
                              env.thisTypeSymbol.fullName.toString,
                              name,
                              tp,
                              anns
                            ) -> true
                        else sir -> false
            case SIR.Var(name, tp, anns) =>
                sir -> false
            case SIR.Let(rec, binding, body, anns) =>
                var bindingIsChanged = false
                val newBinding = binding.map { b =>
                    val (newBody, changed) =
                        specializeSIR(parentSym, b.value, env, possibleOverrides, thisClassNames)
                    if (changed) bindingIsChanged = true
                    Binding(b.name, newBody)
                }
                val (newBody, bodyChanged) =
                    specializeSIR(parentSym, body, env, possibleOverrides, thisClassNames)
                SIR.Let(
                  rec,
                  newBinding,
                  newBody,
                  anns
                ) -> (bindingIsChanged || bodyChanged)
            case SIR.LamAbs(param, term, anns) =>
                val (newTerm, termChanged) =
                    specializeSIR(parentSym, term, env, possibleOverrides, thisClassNames)
                val newSIR = SIR.LamAbs(param, newTerm, anns)
                (newSIR, termChanged)
            case SIR.Apply(f, arg, tp, anns) =>
                val (newF, fChanged) =
                    specializeSIR(parentSym, f, env, possibleOverrides, thisClassNames)
                val (newArg, argChanged) =
                    specializeSIR(parentSym, arg, env, possibleOverrides, thisClassNames)
                val newSIR = SIR.Apply(newF, newArg, tp, anns)
                (newSIR, fChanged || argChanged)
            case SIR.Select(obj, name, tp, anns) =>
                val (newObj, objChanged) =
                    specializeSIR(parentSym, obj, env, possibleOverrides, thisClassNames)
                val newSIR = SIR.Select(newObj, name, tp, anns)
                (newSIR, objChanged)
            case SIR.Const(_, _, _) =>
                sir -> false
            case SIR.And(x, y, anns) =>
                val (newX, xChanged) =
                    specializeSIR(parentSym, x, env, possibleOverrides, thisClassNames)
                val (newY, yChanged) =
                    specializeSIR(parentSym, y, env, possibleOverrides, thisClassNames)
                val newSIR = SIR.And(newX, newY, anns)
                (newSIR, xChanged || yChanged)
            case SIR.Or(x, y, anns) =>
                val (newX, xChanged) =
                    specializeSIR(parentSym, x, env, possibleOverrides, thisClassNames)
                val (newY, yChanged) =
                    specializeSIR(parentSym, y, env, possibleOverrides, thisClassNames)
                val newSIR = SIR.Or(newX, newY, anns)
                (newSIR, xChanged || yChanged)
            case SIR.Not(x, anns) =>
                val (newX, xChanged) =
                    specializeSIR(parentSym, x, env, possibleOverrides, thisClassNames)
                val newSIR = SIR.Not(newX, anns)
                (newSIR, xChanged)
            case SIR.IfThenElse(cond, t, f, tp, anns) =>
                val (newCond, condChanged) =
                    specializeSIR(parentSym, cond, env, possibleOverrides, thisClassNames)
                val (newT, tChanged) =
                    specializeSIR(parentSym, t, env, possibleOverrides, thisClassNames)
                val (newF, fChanged) =
                    specializeSIR(parentSym, f, env, possibleOverrides, thisClassNames)
                val newSIR = SIR.IfThenElse(newCond, newT, newF, tp, anns)
                (newSIR, condChanged || tChanged || fChanged)
            case SIR.Builtin(name, tp, anns) =>
                sir -> false
            case SIR.Error(msg, anns, cause) =>
                sir -> false
            case SIR.Constr(name, dataDecl, args, tp, anns) =>
                var argsAreChanged = false
                val newArgs = args.map { arg =>
                    val (newArg, changed) =
                        specializeSIR(parentSym, arg, env, possibleOverrides, thisClassNames)
                    if changed then argsAreChanged = true
                    newArg
                }
                val newSIR = SIR.Constr(name, dataDecl, newArgs, tp, anns)
                (newSIR, argsAreChanged)
            case SIR.Match(scrutinee, cases, tp, anns) =>
                var casesAreChanged = false
                val newCases = cases.map { c =>
                    val (newCaseBody, changed) =
                        specializeSIR(parentSym, c.body, env, possibleOverrides, thisClassNames)
                    if changed then casesAreChanged = true
                    SIR.Case(c.pattern, newCaseBody)
                }
                val (newScrutinee, scrutineeChanged) =
                    specializeSIR(parentSym, scrutinee, env, possibleOverrides, thisClassNames)
                val newSIR = SIR.Match(newScrutinee, newCases, tp, anns)
                (newSIR, scrutineeChanged || casesAreChanged)
            case SIR.Decl(data, term) =>
                val (newTerm, changed) =
                    specializeSIR(parentSym, term, env, possibleOverrides, thisClassNames)
                SIR.Decl(data, newTerm) -> changed
    }

}

object SIRCompiler {

    case class Env(
        vars: Map[String, SIRType],
        typeVars: Map[Symbol, SIRType],
        debug: Boolean = false,
        level: Int = 0,
        resolvedClasses: Map[Symbol, SIRType] = Map.empty,
        thisTypeSymbol: Symbol = Symbols.NoSymbol
    ) {

        def ++(bindings: Iterable[(String, SIRType)]): Env = copy(vars = vars ++ bindings)

        def +(ntpe: (String, SIRType)): Env = copy(vars = vars + ntpe)

        def withDebug: Env = copy(debug = true)

        def withoutDebug: Env = copy(debug = false)

    }

    object Env {

        def empty: Env = Env(Map.empty, Map.empty)

    }

    val SIRVersion: (Int, Int) = (1, 0)

}
