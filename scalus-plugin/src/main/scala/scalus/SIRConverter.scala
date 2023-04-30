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
  val ConstantListSymbol = requiredModule("scalus.uplc.Constant.List")
  val ConstantPairSymbol = requiredModule("scalus.uplc.Constant.Pair")
  val ByteStringSymbol = requiredModule("scalus.builtins.ByteString")
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

  def mkList(trees: List[Tree], tpt: Tree)(using Context): Tree =
    ref(defn.ListModule)
      .select(nme.apply)
      .appliedToTypeTree(tpt)
      .appliedTo(ctx.typeAssigner.seqToRepeated(SeqLiteral(trees, tpt)))

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
    ref(NamedDeBruijnSymbol.requiredMethod("apply"))
      .appliedToArgs(List(Literal(Constant(name)), Literal(Constant(0))))
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
  def convert(du: DefaultUni): Tree = {
    du match
      case DefaultUni.Integer    => ref(requiredModule("scalus.uplc.DefaultUni.Integer"))
      case DefaultUni.ByteString => ref(requiredModule("scalus.uplc.DefaultUni.ByteString"))
      case DefaultUni.String     => ref(requiredModule("scalus.uplc.DefaultUni.String"))
      case DefaultUni.Unit       => ref(requiredModule("scalus.uplc.DefaultUni.Unit"))
      case DefaultUni.Bool       => ref(requiredModule("scalus.uplc.DefaultUni.Bool"))
      case DefaultUni.Data       => ref(requiredModule("scalus.uplc.DefaultUni.Data"))
      case DefaultUni.ProtoList  => ref(requiredModule("scalus.uplc.DefaultUni.ProtoList"))
      case DefaultUni.ProtoPair  => ref(requiredModule("scalus.uplc.DefaultUni.ProtoPair"))
      case DefaultUni.Apply(f, arg) =>
        ref(requiredModule("scalus.uplc.DefaultUni.Apply")).appliedToArgs(
          List(convert(f), convert(arg))
        )
  }
  def convert(recursivity: Recursivity): Tree = {
    recursivity match
      case Recursivity.Rec    => ref(requiredModule("scalus.sir.Recursivity.Rec"))
      case Recursivity.NonRec => ref(requiredModule("scalus.sir.Recursivity.NonRec"))
  }
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
        ref(ConstantUnitSymbol)
      case uplc.Constant.String(value) =>
        ref(ConstantStringSymbol.requiredMethod("apply")).appliedTo(mkString(value))
      case uplc.Constant.ByteString(value) =>
        ref(ConstantByteStringSymbol.requiredMethod("apply")).appliedTo(convert(value))
      case scalus.uplc.Constant.Integer(value) =>
        ref(ConstantIntegerSymbol.requiredMethod("apply")).appliedTo(convert(value))
      case uplc.Constant.Data(value) =>
        ref(ConstantDataSymbol.requiredMethod("apply")).appliedTo(convert(value))
      case scalus.uplc.Constant.List(elemType, value) =>
        ref(ConstantListSymbol.requiredMethod("apply"))
          .appliedToArgs(List(convert(elemType), mkList(value.map(convert), convert(elemType))))
      case scalus.uplc.Constant.Pair(fst, snd) =>
        ref(ConstantPairSymbol.requiredMethod("apply"))
          .appliedToArgs(List(convert(fst), convert(snd)))
  }

  def mkBigInt(i: BigInt): Tree =
    ref(BigIntSymbol.requiredMethod("apply", List(defn.StringClass.typeRef)))
      .appliedTo(Literal(Constant(i.toString)))

  /* this doesn't work for some reason. ClassTag is not found
      java.lang.NullPointerException:
      at java.base/java.lang.ClassValue.getCacheCarefully(ClassValue.java:190)
      at java.base/java.lang.ClassValue.get(ClassValue.java:103)
      at scala.runtime.ClassValueCompat.get(ClassValueCompat.scala:33)
      at scala.reflect.ClassTag$.apply(ClassTag.scala:157)
   */
  private def ArrayLiteral(values: List[Tree], tpt: Tree)(using Context): Tree =
    val clazzOf = TypeApply(ref(defn.Predef_classOf.termRef), tpt :: Nil)
    val ctag = Apply(TypeApply(ref(defn.ClassTagModule_apply.termRef), tpt :: Nil), clazzOf :: Nil)
    val apply = Select(ref(defn.ArrayModule.termRef), nme.apply)
    TypeApply(apply, tpt :: Nil)
      .appliedTo(ctx.typeAssigner.seqToRepeated(SeqLiteral(values, tpt)))
      .appliedTo(ctag)

  private def ByteArrayLiteral(values: List[Tree])(using Context): Tree =
    val byteType = TypeTree(defn.ByteClass.typeRef)
    val bytesTrees = values.map(b => b.select(nme.toByte))
    val ctag = ref(defn.ClassTagModule).select(nme.Byte)
    val apply = Select(ref(defn.ArrayModule.termRef), nme.apply)
    TypeApply(apply, byteType :: Nil)
      .appliedTo(ctx.typeAssigner.seqToRepeated(SeqLiteral(bytesTrees, byteType)))
      .appliedTo(ctag)

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
    if bs.bytes.isEmpty then ref(ByteStringSymbol.requiredMethod("empty"))
    else
      val byteArr = ByteArrayLiteral(bs.bytes.toList.map(b => Literal(Constant(b))))
      ref(ByteStringSymbol.requiredMethod("fromArray")).appliedTo(byteArr)
  }

  def convert(sir: SIR): Tree = {
    import SIR.*
    println(sir)
    val res = sir match
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
    // println(res)
    // println(res.showIndented(2))
    res
  }
}
