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

    def convertViaSerialization(sir: SIR, span: Spans.Span): Tree = {
        val bitSize =
            scalus.flat.FlatInstantces.SIRHashConsedFlat.bitSizeHC(sir, HashConsed.State.empty)
        val encodedState = HashConsedEncoderState.withSize(bitSize)
        FlatInstantces.SIRHashConsedFlat.encodeHC(sir, encodedState)
        encodedState.encode.filler()
        val bytes = encodedState.encode.result
        val base64 = java.util.Base64.getEncoder.encodeToString(bytes)
        val stringLiteral = Literal(Constant(base64)).withSpan(span)
        val sirToExprFlat = requiredModule("scalus.sir.ToExprHSSIRFlat")
        val decodeBase64SIR = sirToExprFlat.requiredMethod("decodeBase64")
        ref(sirToExprFlat).select(decodeBase64SIR).appliedTo(stringLiteral).withSpan(span)
    }

    def convertSIRToTree(sir: SIR, span: Spans.Span): Tree = {
        val res = convertViaSerialization(sir, span)
        // println(res.showIndented(2))
        res
    }
}
