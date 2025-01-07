package scalus.sir

import scalus.uplc.Constant
import scalus.uplc.DefaultFun

object SirDSL:

    def applyToList(app: SIR): (SIR, List[SIR]) =
        app match
            case SIR.Apply(f, arg, tp) =>
                val (f1, args) = applyToList(f)
                (f1, args :+ arg)
            case f => (f, Nil)

    // flatten LamAbs into a list of names and the body
    def lamAbsToList(lam: SIR): (List[String], SIR) =
        lam match
            case SIR.LamAbs(svar, body) =>
                val (names, body1) = lamAbsToList(body)
                (svar.name :: names, body1)
            case body => (Nil, body)

    def λ(names: String*)(term: SIR): SIR = lam(names*)(term)
    def lam(names: String*)(term: SIR): SIR =
        names.foldRight(term)((e, s) => SIR.LamAbs(SIR.Var(e, s.tp), s))
    extension (term: SIR)
        infix def $(rhs: SIR): SIR =
            SIR.Apply(term, rhs, SIRType.calculateApplyType(term.tp, rhs.tp, Map.empty))

    given Conversion[DefaultFun, SIR] with
        def apply(bn: DefaultFun): SIR = SIRBuiltins.fromUplc(bn)

    given constantAsTerm[A: Constant.LiftValue]: Conversion[A, SIR] with
        def apply(c: A): SIR = {
            val lifted = summon[Constant.LiftValue[A]].lift(c)
            SIR.Const(lifted, SIRType.fromDefaultUni(lifted.tpe))
        }

    given Conversion[Constant, SIR] with
        def apply(c: Constant): SIR =
            c match
                case Constant.Integer(value)    => SIR.Const(c, SIRType.Integer)
                case Constant.ByteString(value) => SIR.Const(c, SIRType.ByteString)
                case Constant.String(value)     => SIR.Const(c, SIRType.String)
                case Constant.Unit              => SIR.Const(c, SIRType.VoidPrimitive)
                case Constant.Bool(value)       => SIR.Const(c, SIRType.BooleanPrimitive)
                case Constant.Data(value)       => SIR.Const(c, SIRType.Data)
                case Constant.List(elemType, value) =>
                    SIR.Const(c, SIRType.List(SIRType.fromDefaultUni(elemType)))
                case Constant.Pair(a, b) =>
                    SIR.Const(
                      c,
                      SIRType.Pair(SIRType.fromDefaultUni(a.tpe), SIRType.fromDefaultUni(b.tpe))
                    )
                case Constant.BLS12_381_G1_Element(value) =>
                    SIR.Const(c, SIRType.BLS12_381_G1_Element)
                case Constant.BLS12_381_G2_Element(value) =>
                    SIR.Const(c, SIRType.BLS12_381_G2_Element)
                case Constant.BLS12_381_MlResult(value) =>
                    SIR.Const(c, SIRType.BLS12_381_MlResult)
