package scalus.sir

import scalus.uplc.Constant
import scalus.uplc.DefaultFun

object SirDSL:
    def applyToList(app: SIRExpr): (SIRExpr, List[SIRExpr]) =
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

    def λ(names: String*)(term: SIRExpr): SIRExpr = lam(names: _*)(term)
    def lam(names: String*)(term: SIRExpr): SIRExpr = names.foldRight(term){
        case (name, body) => SIR.LamAbs(SIR.Var(name, body.tp), body)
    }
    extension (term: SIRExpr) def $(rhs: SIRExpr) = SIR.Apply(term, rhs, SIRType.calculateApplyType(term.tp, rhs.tp, Map.empty))

    given Conversion[DefaultFun, SIRExpr] with
        def apply(bn: DefaultFun): SIRExpr = SIRBuiltins.fromUplc(bn)

    given constantAsTerm[A: Constant.LiftValue]: Conversion[A, SIRExpr] with
        def apply(c: A): SIRExpr = SIR.Const(summon[Constant.LiftValue[A]].lift(c),SIRType.liftM[A])

    given Conversion[Constant, SIRExpr] with
        def apply(c: Constant): SIRExpr =
            c match
                case Constant.Integer(value) => SIR.Const(c, SIRType.IntegerPrimitive)
                case Constant.ByteString(value) => SIR.Const(c, SIRType.ByteStringPrimitive)
                case Constant.String(value) => SIR.Const(c, SIRType.StringPrimitive)
                case Constant.Unit => SIR.Const(c, SIRType.VoidPrimitive)
                case Constant.Bool(value) => SIR.Const(c, SIRType.BooleanPrimitive)
                case Constant.Data(value) => SIR.Const(c, SIRType.Data)
                case Constant.List(elemType, value) => SIR.Const(c, SIRType.List(SIRType.fromDefaultUni(elemType)))
