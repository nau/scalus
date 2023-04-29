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

  val ErrorSymbol = requiredModule("scalus.sir.SIR.Error")
  val ConstSymbol = requiredModule("scalus.sir.SIR.Const")
  val ApplySymbol = requiredModule("scalus.sir.SIR.Apply")
  val BigIntSymbol = requiredModule("scala.BigInt")
  val DataConstrSymbol = requiredModule("scalus.uplc.Data.Constr")
  val DataMapSymbol = requiredModule("scalus.uplc.Data.Map")
  val DataListSymbol = requiredModule("scalus.uplc.Data.List")
  val DataISymbol = requiredModule("scalus.uplc.Data.I")
  val DataBSymbol = requiredModule("scalus.uplc.Data.B")
  val ConstantIntegerSymbol = requiredModule("scalus.uplc.Constant.Integer")
  val ConstantBoolSymbol = requiredModule("scalus.uplc.Constant.Bool")
  val ConstantUnitSymbol = requiredModule("scalus.uplc.Constant.Unit")
  val ConstantStringSymbol = requiredModule("scalus.uplc.Constant.String")
  val ConstantByteStringSymbol = requiredModule("scalus.uplc.Constant.ByteString")
  val ConstantDataSymbol = requiredModule("scalus.uplc.Constant.Data")
  val ByteStringSymbol = requiredModule("scalus.uplc.ByteString")
  val VarSymbol = requiredModule("scalus.sir.SIR.Var")
  val LetSymbol = requiredModule("scalus.sir.SIR.Let")
  val LamAbsSymbol = requiredModule("scalus.sir.SIR.LamAbs")
  val NamedDeBruijnSymbol = requiredModule("scalus.uplc.NamedDeBruijn")
  val BuiltinSymbol = requiredModule("scalus.sir.SIR.Builtin")
  val BindingSymbol = requiredModule("scalus.sir.Binding")
  val BindingClassSymbol = requiredClass("scalus.sir.Binding")
  val MatchSymbol = requiredModule("scalus.sir.SIR.Match")
  val CaseSymbol = requiredModule("scalus.sir.Case")
  val CaseClassSymbol = requiredClass("scalus.sir.Case")
  val ConstrDeclSymbol = requiredModule("scalus.sir.ConstrDecl")
  val ConstrSymbol = requiredModule("scalus.sir.SIR.Constr")
  val SIRClassSymbol = requiredClass("scalus.sir.SIR")
  val DataClassSymbol = requiredClass("scalus.uplc.Data")
  val DataDeclSymbol = requiredModule("scalus.sir.DataDecl")
  val DeclSymbol = requiredModule("scalus.sir.SIR.Decl")
  val IfThenElseSymbol = requiredModule("scalus.sir.SIR.IfThenElse")
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

  def mkLet(recursivity: Recursivity, bindings: List[Binding], body: SIR) =
    ref(LetSymbol.requiredMethod("apply"))
      .appliedToArgs(
        List(
          convert(recursivity),
          mkList(bindings.map(convert), TypeTree(BindingClassSymbol.typeRef)),
          convert(body)
        )
      )

  def mkMatch(scrutinee: SIR, cases: List[Case]) =
    ref(MatchSymbol.requiredMethod("apply"))
      .appliedToArgs(
        List(
          convert(scrutinee),
          mkList(cases.map(convert), TypeTree(CaseClassSymbol.typeRef))
        )
      )

  def mkCase(cs: Case) =
    ref(CaseSymbol.requiredMethod("apply"))
      .appliedToArgs(
        List(
          mkConstrDecl(cs.constr),
          mkList(cs.bindings.map(mkString), TypeTree(defn.StringClass.typeRef)),
          convert(cs.body)
        )
      )

  def mkConstrDecl(constr: ConstrDecl) =
    ref(ConstrDeclSymbol.requiredMethod("apply")).appliedToArgs(
      List(
        mkString(constr.name),
        mkList(constr.params.map(mkString), TypeTree(defn.StringClass.typeRef))
      )
    )

  def mkDataDecl(data: DataDecl) =
    ref(DataDeclSymbol.requiredMethod("apply")).appliedToArgs(
      List(
        mkString(data.name),
        mkList(data.constructors.map(mkConstrDecl), TypeTree(ConstrDeclSymbol.typeRef))
      )
    )
  def mkConstr(name: String, data: DataDecl, args: List[SIR]) =
    ref(ConstrSymbol.requiredMethod("apply"))
      .appliedToArgs(
        List(
          mkString(name),
          mkDataDecl(data),
          mkList(args.map(convert), TypeTree(SIRClassSymbol.typeRef))
        )
      )
  def mkDecl(data: DataDecl, term: SIR) =
    ref(DeclSymbol.requiredMethod("apply"))
      .appliedToArgs(List(mkDataDecl(data), convert(term)))

  def mkIfThenElse(cond: SIR, thenp: SIR, elsep: SIR) =
    ref(IfThenElseSymbol.requiredMethod("apply"))
      .appliedToArgs(List(convert(cond), convert(thenp), convert(elsep)))

  def mkString(s: String) = Literal(Constant(s))
  def mkError(msg: String) =
    ref(ErrorSymbol.requiredMethod("apply")).appliedTo(mkString(msg))

  def convert(cs: Case): Tree = mkCase(cs)
  def convert(fun: DefaultFun): Tree = mkDefaultFun(fun)
  def convert(recursivity: Recursivity): Tree = ???
  def convert(binding: Binding): Tree = {
    ref(BindingSymbol.requiredMethod("apply"))
      .appliedToArgs(List(Literal(Constant(binding.name)), convert(binding.value)))
  }
  def convert(name: NamedDeBruijn): Tree = mkNamedDeBruijn(name.name)
  def convert(value: Boolean): Tree = Literal(Constant(value))
  def convert(const: scalus.uplc.Constant): Tree = {
    const match
      case uplc.Constant.Bool(value) =>
        ref(ConstantBoolSymbol.requiredMethod("apply")).appliedTo(convert(value))
      case uplc.Constant.Unit =>
        ref(ConstantUnitSymbol.requiredMethod("apply"))
      case uplc.Constant.String(value) =>
        ref(ConstantStringSymbol.requiredMethod("apply")).appliedTo(mkString(value))
      case uplc.Constant.ByteString(value) =>
        ref(ConstantByteStringSymbol.requiredMethod("apply")).appliedTo(convert(value))
      case scalus.uplc.Constant.Integer(value) =>
        ref(ConstantIntegerSymbol.requiredMethod("apply")).appliedTo(convert(value))
      case uplc.Constant.Data(value) =>
        ref(ConstantDataSymbol.requiredMethod("apply")).appliedTo(convert(value))
  }

  def mkBigInt(i: BigInt): Tree =
    ref(BigIntSymbol.requiredMethod("apply")).appliedTo(Literal(Constant(i.toString)))

  private def ArrayLiteral(values: List[Tree], tpt: Tree)(using Context): Tree =
    val clazzOf = TypeApply(ref(defn.Predef_classOf.termRef), tpt :: Nil)
    val ctag = Apply(TypeApply(ref(defn.ClassTagModule_apply.termRef), tpt :: Nil), clazzOf :: Nil)
    val apply = Select(ref(defn.ArrayModule.termRef), nme.apply)
    Apply(Apply(TypeApply(apply, tpt :: Nil), values), ctag :: Nil)

  def convert(i: BigInt): Tree = mkBigInt(i)

  def convert(d: uplc.Data): Tree = {
    d match
      case uplc.Data.Constr(i, args) =>
        ref(DataConstrSymbol.requiredMethod("apply")).appliedToArgs(
          List(Literal(Constant(i)), mkList(args.map(convert), TypeTree(DataClassSymbol.typeRef)))
        )
      case uplc.Data.I(i) =>
        ref(DataISymbol.requiredMethod("apply")).appliedToArgs(List(convert(i)))
      case uplc.Data.B(b) =>
        ref(DataBSymbol.requiredMethod("apply")).appliedToArgs(List(convert(b)))
      case uplc.Data.List(l) =>
        ref(DataListSymbol.requiredMethod("apply"))
          .appliedToArgs(List(mkList(l.map(convert), TypeTree(DataClassSymbol.typeRef))))
      case uplc.Data.Map(m) => ??? // TODO
  }

  def convert(bs: ByteString) = {
    val byteArr =
      ArrayLiteral(bs.bytes.toList.map(b => Literal(Constant(b))), TypeTree(defn.ByteClass.typeRef))
    ref(ByteStringSymbol.requiredMethod("apply")).appliedTo(byteArr)
  }

  def convert(sir: SIR): Tree = {
    import SIR.*
    sir match
      case Error(msg)               => mkError(msg)
      case Var(name)                => mkVar(name)
      case Const(const)             => mkConst(const)
      case Apply(f, arg)            => mkApply(f, arg)
      case LamAbs(name, term)       => mkLamAbs(name, term)
      case Builtin(bn)              => mkBuiltin(bn)
      case Let(rec, bindings, body) => mkLet(rec, bindings, body)
      case Match(scrutinee, cases)  => mkMatch(scrutinee, cases)
      case Constr(name, data, args) => mkConstr(name, data, args)
      case Decl(data, term)         => mkDecl(data, term)
      case IfThenElse(cond, t, f)   => mkIfThenElse(cond, t, f)
  }
}
