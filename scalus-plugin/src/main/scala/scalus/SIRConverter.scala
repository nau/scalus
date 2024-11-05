package scalus

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.NameKinds.UniqueNameKind
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.MethodType
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Types.TypeRef
import dotty.tools.dotc.core.*
import dotty.tools.dotc.util.Spans
import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.flat.FlatInstantces
import scalus.sir.Binding
import scalus.sir.ConstrDecl
import scalus.sir.DataDecl
import scalus.sir.Recursivity
import scalus.sir.SIR
import scalus.sir.SIR.Case
import scalus.sir.SIRType
import scalus.uplc.DefaultFun
import scalus.uplc.DefaultUni
import scalus.utils.HashConsed
import scalus.utils.HashConsedEncoderState
import scalus.utils.HashConsedDecoderState
import scalus.utils.HashConsedFlat

import scala.collection.immutable
import scala.language.implicitConversions
import scalus.builtin.BLS12_381_G1_Element
import scalus.builtin.BLS12_381_G2_Element


class SIRConverter(using Context) {
    import tpd.*

    val ErrorSymbol = requiredModule("scalus.sir.SIR.Error")
    val ConstSymbol = requiredModule("scalus.sir.SIR.Const")
    val ApplySymbol = requiredModule("scalus.sir.SIR.Apply")
    val BigIntSymbol = requiredModule("scala.math.BigInt")
    val BigIntClassSymbol = requiredClass("scala.math.BigInt")
    val DataConstrSymbol = requiredModule("scalus.builtin.Data.Constr")
    val DataMapSymbol = requiredModule("scalus.builtin.Data.Map")
    val DataListSymbol = requiredModule("scalus.builtin.Data.List")
    val DataISymbol = requiredModule("scalus.builtin.Data.I")
    val DataBSymbol = requiredModule("scalus.builtin.Data.B")
    val ConstantClassSymbol = requiredClass("scalus.uplc.Constant")
    val ConstantIntegerSymbol = requiredModule("scalus.uplc.Constant.Integer")
    val ConstantBoolSymbol = requiredModule("scalus.uplc.Constant.Bool")
    val ConstantUnitSymbol = requiredModule("scalus.uplc.Constant.Unit")
    val ConstantStringSymbol = requiredModule("scalus.uplc.Constant.String")
    val ConstantByteStringSymbol = requiredModule("scalus.uplc.Constant.ByteString")
    val ConstantDataSymbol = requiredModule("scalus.uplc.Constant.Data")
    val ConstantListSymbol = requiredModule("scalus.uplc.Constant.List")
    val ConstantPairSymbol = requiredModule("scalus.uplc.Constant.Pair")
    val ConstantBLS12_381_G1_ElementSymbol = requiredModule(
      "scalus.uplc.Constant.BLS12_381_G1_Element"
    )
    val ConstantBLS12_381_G2_ElementSymbol = requiredModule(
      "scalus.uplc.Constant.BLS12_381_G2_Element"
    )
    val ByteStringSymbol = requiredModule("scalus.builtin.ByteString")
    val ByteStringClassSymbol = requiredClass("scalus.builtin.ByteString")
    val BLS12_381_G1_ElementSymbol = requiredModule("scalus.builtin.BLS12_381_G1_Element")
    val BLS12_381_G2_ElementSymbol = requiredModule("scalus.builtin.BLS12_381_G2_Element")
    val VarSymbol = requiredModule("scalus.sir.SIR.Var")
    val ExternalVarSymbol = requiredModule("scalus.sir.SIR.ExternalVar")
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
    val ConstrDeclClassSymbol = requiredClass("scalus.sir.ConstrDecl")
    val ConstrSymbol = requiredModule("scalus.sir.SIR.Constr")
    val SIRClassSymbol = requiredClass("scalus.sir.SIR")
    val DataClassSymbol = requiredClass("scalus.builtin.Data")
    val DataDeclSymbol = requiredModule("scalus.sir.DataDecl")
    val DeclSymbol = requiredModule("scalus.sir.SIR.Decl")
    val IfThenElseSymbol = requiredModule("scalus.sir.SIR.IfThenElse")
    val AndSymbol = requiredModule("scalus.sir.SIR.And")
    val OrSymbol = requiredModule("scalus.sir.SIR.Or")
    val NotSymbol = requiredModule("scalus.sir.SIR.Not")

    val letDefName: UniqueNameKind = new UniqueNameKind("$letDef")
    val letDefFlags: FlagSet = Synthetic | Method

    /*
    This is a modification of Dotty mkList method.
    The original method generates JavaSeqLiteral, which doesn't work for some reason.
    Probably it's because of the phase order.
     */
    def mkList(trees: List[Tree], tpt: Tree)(using Context): Tree =
        ref(defn.ListModule)
            .select(nme.apply)
            .appliedToTypeTree(tpt)
            .appliedTo(ctx.typeAssigner.seqToRepeated(SeqLiteral(trees, tpt)))

    def mkApply(f: SIR, arg: SIR, tp: SIRType) =
        ref(ApplySymbol.requiredMethod("apply")).appliedToArgs(List(convert(f), convert(arg), mkSIRType(tp)))
    def mkLamAbs(param: SIR.Var, term: SIR) =
        ref(LamAbsSymbol.requiredMethod("apply"))
            .appliedToArgs(List(mkVar(param.name, param.tp), convert(term)))
    def mkVar(name: String, tp: SIRType) =
        ref(VarSymbol.requiredMethod("apply")).appliedTo(mkString(name), mkSIRType(tp))
    def mkExternalVar(modName: String, name: String, tp: SIRType) =
        ref(ExternalVarSymbol.requiredMethod("apply")).appliedTo(mkString(modName), mkString(name), mkSIRType(tp))
    def mkConst(const: scalus.uplc.Constant, tp: SIRType) =
        ref(ConstSymbol.requiredMethod("apply")).appliedTo(convert(const), mkSIRType(tp))
    def mkBuiltin(bn: DefaultFun, tp: SIRType) =
        ref(BuiltinSymbol.requiredMethod("apply")).appliedTo(convert(bn), mkSIRType(tp))
    def mkDefaultFun(fun: DefaultFun) = ref(requiredModule(s"scalus.uplc.DefaultFun.$fun"))

    def mkSIRType(tp: SIRType): Tree = {
        ???
    }

    /*  Generate a method for each let binding to avoid a Java limit on the size of a method of 64KB
      which arises when scripts are too large
      so that for each SIR.Let(foo, ...) we generate:
      {
        def foo$letDef: SIR = SIR.Let(foo, ...)
        foo
      }
     */
    def mkLetDef(recursivity: Recursivity, bindings: List[Binding], body: SIR) =
        val x = mkLet(recursivity, bindings, body)
        val tpe = SIRClassSymbol.typeRef
        val defName = letDefName.fresh(bindings.head.name.toTermName)
        val defSym = newSymbol(ctx.owner, defName, letDefFlags, MethodType(Nil, tpe))
        tpd.Block(List(tpd.DefDef(defSym, Nil, tpe, x)), ref(defSym))

    def mkLet(recursivity: Recursivity, bindings: List[Binding], body: SIR): Tree =
        ref(LetSymbol.requiredMethod("apply"))
            .appliedToArgs(
              List(
                convert(recursivity),
                mkList(bindings.map(convert), TypeTree(BindingClassSymbol.typeRef)),
                convert(body)
              )
            )

    def mkMatch(scrutinee: SIR, cases: List[Case], tp: SIRType) =
        ref(MatchSymbol.requiredMethod("apply"))
            .appliedToArgs(
              List(
                convert(scrutinee),
                mkList(cases.map(convert), TypeTree(CaseClassSymbol.typeRef)),
                mkSIRType(tp)
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
            mkList(constr.params.map(b => mkString(b.name)), TypeTree(defn.StringClass.typeRef))
          )
        )

    def mkDataDecl(data: DataDecl) =
        ref(DataDeclSymbol.requiredMethod("apply")).appliedToArgs(
          List(
            mkString(data.name),
            mkList(data.constructors.map(mkConstrDecl), TypeTree(ConstrDeclClassSymbol.typeRef))
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

    def mkIfThenElse(cond: SIR, thenp: SIR, elsep: SIR, tp: SIRType) =
        ref(IfThenElseSymbol.requiredMethod("apply"))
            .appliedToArgs(List(convert(cond), convert(thenp), convert(elsep), mkSIRType(tp)))

    def mkString(s: String) = Literal(Constant(s))
    def mkError(msg: String) =
        ref(ErrorSymbol.requiredMethod("apply")).appliedTo(mkString(msg))

    def mkAnd(left: SIR, right: SIR) =
        ref(AndSymbol.requiredMethod("apply")).appliedToArgs(List(convert(left), convert(right)))

    def mkOr(left: SIR, right: SIR) =
        ref(OrSymbol.requiredMethod("apply")).appliedToArgs(List(convert(left), convert(right)))

    def mkNot(term: SIR) =
        ref(NotSymbol.requiredMethod("apply")).appliedTo(convert(term))

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
            case DefaultUni.BLS12_381_G1_Element =>
                ref(requiredModule("scalus.uplc.DefaultUni.BLS12_381_G1_Element"))
            case DefaultUni.BLS12_381_G2_Element =>
                ref(requiredModule("scalus.uplc.DefaultUni.BLS12_381_G2_Element"))
            case DefaultUni.BLS12_381_MlResult =>
                ref(requiredModule("scalus.uplc.DefaultUni.BLS12_381_MlResult"))
            case DefaultUni.ProtoList => ref(requiredModule("scalus.uplc.DefaultUni.ProtoList"))
            case DefaultUni.ProtoPair => ref(requiredModule("scalus.uplc.DefaultUni.ProtoPair"))
            case DefaultUni.Apply(f, arg) =>
                ref(requiredModule("scalus.uplc.DefaultUni.Apply").requiredMethod("apply"))
                    .appliedToArgs(
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
            case uplc.Constant.Integer(value) =>
                ref(ConstantIntegerSymbol.requiredMethod("apply")).appliedTo(convert(value))
            case uplc.Constant.Data(value) =>
                ref(ConstantDataSymbol.requiredMethod("apply")).appliedTo(convert(value))
            case uplc.Constant.List(elemType, value) =>
                ref(ConstantListSymbol.requiredMethod("apply"))
                    .appliedToArgs(
                      List(
                        convert(elemType),
                        mkList(value.map(convert), TypeTree(ConstantClassSymbol.typeRef))
                      )
                    )
            case uplc.Constant.Pair(fst, snd) =>
                ref(ConstantPairSymbol.requiredMethod("apply"))
                    .appliedToArgs(List(convert(fst), convert(snd)))
            case uplc.Constant.BLS12_381_G1_Element(value) =>
                ref(ConstantBLS12_381_G1_ElementSymbol.requiredMethod("apply"))
                    .appliedTo(convert(value))
            case uplc.Constant.BLS12_381_G2_Element(value) =>
                ref(ConstantBLS12_381_G2_ElementSymbol.requiredMethod("apply"))
                    .appliedTo(convert(value))
            case uplc.Constant.BLS12_381_MlResult(value) => ??? // should not happen
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

  private def ArrayLiteral(values: List[Tree], tpt: Tree)(using Context): Tree =
    val clazzOf = TypeApply(ref(defn.Predef_classOf.termRef), tpt :: Nil)
    val ctag = Apply(TypeApply(ref(defn.ClassTagModule_apply.termRef), tpt :: Nil), clazzOf :: Nil)
    val apply = Select(ref(defn.ArrayModule.termRef), nme.apply)
    TypeApply(apply, tpt :: Nil)
      .appliedTo(ctx.typeAssigner.seqToRepeated(SeqLiteral(values, tpt)))
      .appliedTo(ctag)
     */

    private def ByteArrayLiteral(values: List[Tree])(using Context): Tree =
        val byteType = TypeTree(defn.ByteClass.typeRef)
        val bytesTrees = values.map(b => b.select(nme.toByte))
        val ctag = ref(defn.ClassTagModule).select(nme.Byte)
        val apply = Select(ref(defn.ArrayModule.termRef), nme.apply)
        TypeApply(apply, byteType :: Nil)
            .appliedTo(ctx.typeAssigner.seqToRepeated(SeqLiteral(bytesTrees, byteType)))
            .appliedTo(ctag)

    def convert(i: BigInt): Tree = mkBigInt(i)

    def convert(d: Data): Tree = {
        d match
            case Data.Constr(i, args) =>
                ref(DataConstrSymbol.requiredMethod("apply")).appliedToArgs(
                  List(
                    Literal(Constant(i)),
                    mkList(args.map(convert), TypeTree(DataClassSymbol.typeRef))
                  )
                )
            case Data.I(i) =>
                ref(DataISymbol.requiredMethod("apply")).appliedToArgs(List(convert(i)))
            case Data.B(b) =>
                ref(DataBSymbol.requiredMethod("apply")).appliedToArgs(List(convert(b)))
            case Data.List(l) =>
                ref(DataListSymbol.requiredMethod("apply"))
                    .appliedToArgs(List(mkList(l.map(convert), TypeTree(DataClassSymbol.typeRef))))
            case Data.Map(m) => ??? // TODO
    }

    def convert(bs: ByteString) = {
        if bs.bytes.isEmpty then ref(ByteStringSymbol.requiredMethod("empty"))
        else
            val byteArr = ByteArrayLiteral(bs.bytes.toList.map(b => Literal(Constant(b))))
            ref(ByteStringSymbol.requiredMethod("fromArray")).appliedTo(byteArr)
    }

    def convert(bs: BLS12_381_G1_Element): Tree = {
        ref(BLS12_381_G1_ElementSymbol.requiredMethod("apply")).appliedTo(convert(bs.value))
    }

    def convert(bs: BLS12_381_G2_Element): Tree = {
        ref(BLS12_381_G2_ElementSymbol.requiredMethod("apply")).appliedTo(convert(bs.value))
    }

    def convert(sir: SIR): Tree = {
        import SIR.*
        val res = sir match
            case Error(msg, _)              => mkError(msg)
            case Var(name, tp)              => mkVar(name, tp)
            case ExternalVar(modName, name, tp) => mkExternalVar(modName, name, tp)
            case Const(const,tp)            => mkConst(const, tp)
            case Apply(f, arg, tp)          => mkApply(f, arg, tp)
            case LamAbs(name, term)         => mkLamAbs(name, term)
            case Builtin(bn, tp)            => mkBuiltin(bn, tp)
            case Let(rec, bindings, body)   => mkLetDef(rec, bindings, body)
            case Match(scrutinee, cases,tp) => mkMatch(scrutinee, cases, tp)
            case Constr(name, data, args)   => mkConstr(name, data, args)
            case Decl(data, term)           => mkDecl(data, term)
            case And(t1, t2)                => mkAnd(t1, t2)
            case Or(t1, t2)                 => mkOr(t1, t2)
            case Not(t)                     => mkNot(t)
            case IfThenElse(cond, t, f, tp) => mkIfThenElse(cond, t, f, tp)
        // println(res)
        // println(res.showIndented(2))
        res
    }


    def convertViaSerialization(sir: SIR, span: Spans.Span): Tree = {
        val bitSize = scalus.flat.FlatInstantces.SIRHashConsedFlat.bitSizeHC(sir, HashConsed.State.empty)
        val encodedState = HashConsedEncoderState.withSize(bitSize)
        FlatInstantces.SIRHashConsedFlat.encodeHC(sir, encodedState)
        encodedState.encode.filler()
        val bytes = encodedState.encode.result
        val base64 = java.util.Base64.getEncoder.encodeToString(bytes)
        // TODO: set pos.
        val stringLiteral = Literal(Constant(base64)).withSpan(span)
        //val byteArr = ByteArrayLiteral(bytes.toList.map(b => Literal(Constant(b))))
        val sirToExprFlat = requiredModule("scalus.sir.ToExprHSSIRFlat")
        val decodeBase64SIR = sirToExprFlat.requiredMethod("decodeBase64")
        ref(sirToExprFlat).select(decodeBase64SIR).appliedTo(stringLiteral).withSpan(span)
    }

    def convertSIRToTree(sir: SIR, span: Spans.Span): Tree = {
        //val res = convert(sir)
        val res = convertViaSerialization(sir, span)
        // println(res)
        // println(s"convertSIRToTree: ${ctx.source} ${ctx.phase}")
        // println(res.showIndented(2))
        res
    }
}
