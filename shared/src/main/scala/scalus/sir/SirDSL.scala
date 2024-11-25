package scalus.sir

import scalus.uplc.Constant
import scalus.uplc.DefaultFun
import scalus.pretty

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

    def λ(names: String*)(term: SIRExpr): SIRExpr = lam(names*)(term)
    def lam(names: String*)(term: SIRExpr): SIRExpr =
        names.foldRight(term)((e, s) => SIR.LamAbs(SIR.Var(e, s.tp), s))
    extension (term: SIRExpr)
        infix def $(rhs: SIRExpr): SIRExpr =
            println(s"Applying $term to $rhs")
            println(s"fun: ${term.pretty.render(100)}")
            println(s"fun.tp: ${term.tp.show}")
            println(s"arg: ${rhs.pretty.render(100)}")
            println(s"arg.tp: ${rhs.tp.show}")
            SIR.Apply(term, rhs, SIRType.calculateApplyType(term.tp, rhs.tp, Map.empty))

    given Conversion[DefaultFun, SIRExpr] with
        def apply(bn: DefaultFun): SIRExpr = SIRBuiltins.fromUplc(bn)

    given constantAsTerm[A: Constant.LiftValue]: Conversion[A, SIRExpr] with
        def apply(c: A): SIRExpr = {
            val lifted = summon[Constant.LiftValue[A]].lift(c)
            SIR.Const(lifted, SIRType.fromDefaultUni(lifted.tpe))
        }

    given Conversion[Constant, SIRExpr] with
        def apply(c: Constant): SIRExpr =
            c match
                case Constant.Integer(value)    => SIR.Const(c, SIRType.IntegerPrimitive)
                case Constant.ByteString(value) => SIR.Const(c, SIRType.ByteStringPrimitive)
                case Constant.String(value)     => SIR.Const(c, SIRType.StringPrimitive)
                case Constant.Unit              => SIR.Const(c, SIRType.VoidPrimitive)
                case Constant.Bool(value)       => SIR.Const(c, SIRType.BooleanPrimitive)
                case Constant.Data(value)       => SIR.Const(c, SIRType.Data)
                case Constant.List(elemType, value) =>
                    SIR.Const(c, SIRType.List(SIRType.fromDefaultUni(elemType)))
